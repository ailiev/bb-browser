package main

import (
	"context"
	"log"
	"net/http"
	"strconv"

	remoteexecution "github.com/bazelbuild/remote-apis/build/bazel/remote/execution/v2"
	"github.com/buildbarn/bb-storage/pkg/cas"
	"github.com/buildbarn/bb-storage/pkg/util"
	"github.com/golang/protobuf/jsonpb"
	"github.com/golang/protobuf/proto"
	"github.com/gorilla/mux"
)

type APIService struct {
	marshaler jsonpb.Marshaler
}

func NewAPIService(contentAddressableStorage cas.ContentAddressableStorage, router *mux.Router) *APIService {
	s := &APIService{}
	router.HandleFunc("/api/get_command", s.handleGetObject(
		func(ctx context.Context, digest *util.Digest) (proto.Message, error) {
			return contentAddressableStorage.GetCommand(ctx, digest)
		}))
	router.HandleFunc("/api/get_directory", s.handleGetObject(
		func(ctx context.Context, digest *util.Digest) (proto.Message, error) {
			return contentAddressableStorage.GetDirectory(ctx, digest)
		}))
	router.HandleFunc("/api/get_tree", s.handleGetObject(
		func(ctx context.Context, digest *util.Digest) (proto.Message, error) {
			return contentAddressableStorage.GetTree(ctx, digest)
		}))
	return s
}

func getDigestFromQueryParameters(req *http.Request) (*util.Digest, error) {
	vars := req.URL.Query()
	sizeBytes, err := strconv.ParseInt(vars.Get("size_bytes"), 10, 64)
	if err != nil {
		return nil, err
	}
	return util.NewDigest(
		vars.Get("instance"),
		&remoteexecution.Digest{
			Hash:      vars.Get("hash"),
			SizeBytes: sizeBytes,
		})
}

func (s *APIService) handleGetObject(getter func(ctx context.Context, digest *util.Digest) (proto.Message, error)) func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		digest, err := getDigestFromQueryParameters(req)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		message, err := getter(req.Context(), digest)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if err := s.marshaler.Marshal(w, message); err != nil {
			log.Print(err)
		}
	}
}

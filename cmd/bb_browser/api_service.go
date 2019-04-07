package main

import (
	"log"
	"strconv"
	"net/http"

	remoteexecution "github.com/bazelbuild/remote-apis/build/bazel/remote/execution/v2"
	"github.com/buildbarn/bb-storage/pkg/cas"
	"github.com/buildbarn/bb-storage/pkg/util"
	"github.com/golang/protobuf/jsonpb"
	"github.com/gorilla/mux"
)

type APIService struct {
	contentAddressableStorage cas.ContentAddressableStorage
	marshaler                 jsonpb.Marshaler
}

func NewAPIService(contentAddressableStorage cas.ContentAddressableStorage, router *mux.Router) *APIService {
	s := &APIService{
		contentAddressableStorage: contentAddressableStorage,
	}
	router.HandleFunc("/api/get_directory", s.handleGetDirectory)
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

func (s *APIService) handleGetDirectory(w http.ResponseWriter, req *http.Request) {
	digest, err := getDigestFromQueryParameters(req)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	directory, err := s.contentAddressableStorage.GetDirectory(req.Context(), digest)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	if err := s.marshaler.Marshal(w, directory); err != nil {
		log.Print(err)
	}
}

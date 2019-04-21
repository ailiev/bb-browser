package main

import (
	"net/http"

	"github.com/buildbarn/bb-browser/cmd/bb_browser/assets"
	"github.com/gorilla/mux"
)

type AssetService struct {
}

func NewAssetService(router *mux.Router) *AssetService {
	s := &AssetService{}
	router.HandleFunc("/",
		s.handleRequest(assets.IndexHTML, "text/html; charset=utf-8"))
	router.HandleFunc("/bootstrap.css",
		s.handleRequest(assets.BootstrapCSS, "text/css"))
	router.HandleFunc("/bootstrap.js",
		s.handleRequest(assets.BootstrapJS, "application/javascript"))
	router.HandleFunc("/frontend.js",
		s.handleRequest(assets.FrontendJS, "application/javascript"))
	router.HandleFunc("/jquery.js",
		s.handleRequest(assets.JQueryJS, "application/javascript"))
	router.HandleFunc("/terminal.css",
		s.handleRequest(assets.TerminalCSS, "text/css"))
	return s
}

func (s *AssetService) handleRequest(body []byte, contentType string) func(http.ResponseWriter, *http.Request) {
	return func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", contentType)
		w.Write(body)
	}
}

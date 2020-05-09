#!/bin/bash

set -euf -o pipefail

GO111MODULE=on go get github.com/cortesi/modd/cmd/modd
go get -u github.com/kyoh86/richgo
GO111MODULE=on go get github.com/golangci/golangci-lint/cmd/golangci-lint@v1.26.0

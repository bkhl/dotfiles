IMAGE := docker.io/library/alpine:latest

define run
podman run --rm --interactive \
--volume "$(CURDIR):$(CURDIR):z" \
--workdir "$(CURDIR)"
endef

.PHONY: help image build

all:
	make --print-targets

image:
	podman build -f Containerfile -t $(IMAGE) .

build: image
	$(run) $(IMAGE) bash build.sh

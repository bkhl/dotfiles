MAKEFLAGS += --always-make

IMAGE := iosevka-builder
SRC := src
BUILD := dist
VERSION = v33.1.0

all:
	make --print-targets

clean:
	-podman image rm $(IMAGE)
	-rm -rf $(SRC) $(BUILD)

image:
	git clone --branch $(VERSION) --depth=1 https://github.com/be5invis/Iosevka.git $(SRC)
	podman build -t $(IMAGE) -f $(SRC)/docker/Dockerfile $(SRC)/docker

fonts:
	podman run --cpus=1 --rm --tty --volume .:/work:z $(IMAGE) ttf::iosevka-bkhl-{sans,serif}-{normal,fixed}

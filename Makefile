IMAGE_NAME=dimchansky-github-io-builder

build-image:
	DOCKER_BUILDKIT=1 docker build -t $(IMAGE_NAME) .

build-site:
	docker run --rm \
	  -v "$(PWD):/app" \
	  $(IMAGE_NAME) \
	  build

watch-site:
	docker run --rm \
	  -v "$(PWD):/app" \
	  -p 8080:8080 \
	  $(IMAGE_NAME) \
	  watch --host "0.0.0.0" --port 8080

clean-caches:
	docker builder prune --force

.PHONY: build-image build-site
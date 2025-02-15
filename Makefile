IMAGE_NAME=ghcr.io/dimchansky/dimchansky-github-io-builder

.PHONY: all
all: build-image build-site

.PHONY: build-image
build-image:
	DOCKER_BUILDKIT=1 docker build -t $(IMAGE_NAME) .

.PHONY: build-site
build-site:
	docker run --rm \
	  -v "$(PWD):/app" \
	  $(IMAGE_NAME) \
	  rebuild

.PHONY: push-image
push-image: build-image
	docker push $(IMAGE_NAME):latest

.PHONY: watch-site
watch-site:
	docker run --rm \
	  -v "$(PWD):/app" \
	  -p 8080:8080 \
	  $(IMAGE_NAME) \
	  watch --host "0.0.0.0" --port 8080

.PHONY: clean-caches
clean-caches:
	docker builder prune --force

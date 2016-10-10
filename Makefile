.PHONY: dev deploy clean

dev:
	bundle exec middleman serve

build:
	bundle exec middleman build

.deployed: build
	@echo "Not yet!"

deploy: .deployed
	@echo "Not yet!"

clean:
	rm -rf build .deployed

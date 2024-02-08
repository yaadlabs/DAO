# Build and run the tests specified in dao/dao-specs
run-tests:
	@rm -rf ./result
	@nix build .#dao-test
	@./result/bin/dao-test
	@rm -rf ./result
	@rm -rf ./dist-newstyle

# Build the 'dao-lib' library
run-build:
	@rm -rf ./result
	@nix build .#dao-lib
	@rm -rf ./result
	@rm -rf ./dist-newstyle

# Build and run 'create-scripts'
run-create-scripts:
	@rm -rf ./result
	@nix build .#dao-scripts
	@./result/bin/create-scripts compile
	@rm -rf ./result
	@rm -rf ./dist-newstyle

# Build but don't run 'create-scripts', for testing it compiles
build-create-scripts:
	@rm -rf ./result
	@nix build .#dao-scripts
	@rm -rf ./result
	@rm -rf ./dist-newstyle

clean:
	@rm -rf ./result
	@rm -rf ./dao/dist-newstyle
	@rm -rf ./dist-newstyle

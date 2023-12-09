# Build and run the tests specified in dao/dao-specs
run-tests:
	@rm -rf ./result
	@nix build .#dao-test
	@./result/bin/dao-test
	@rm -rf ./result

clean:
	@rm -rf ./result
	@rm -rf ./dao/dist-newstyle

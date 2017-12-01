with (import <nixpkgs> {});
let env = bundlerEnv {
  name = "custom-jekyll";   
  inherit ruby;
  gemfile = ./Gemfile;
  lockfile = ./Gemfile.lock;
  gemset = ./gemset.nix;
};
in stdenv.mkDerivation {
  name = "custom-jekyll";
  buildInputs = [env ruby];
}

{ nix-gitignore, python3Packages }:

let
  gen_ci = { buildPythonPackage, python-gitlab, pyyaml }:
    buildPythonPackage {
      pname = "gen_ci";
      version = "0.0.1";
      src = nix-gitignore.gitignoreSource [] ./.;
      propagatedBuildInputs = [ python-gitlab pyyaml ];
      preferLocalBuild = true;
    };
in
python3Packages.callPackage gen_ci { }

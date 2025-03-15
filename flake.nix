{
  inputs = {
      nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

      raylib = {
          url = "github:Anut-py/h-raylib";
          inputs.nixpkgs.follows = "nixpkgs";
      };
  };

  outputs = { self, nixpkgs, raylib, ...}@inputs: {
      packages.x86_64-linux.default = raylib.outputs.packages.x86_64-linux.default;
      devShells.x86_64-linux.default = raylib.outputs.devShells.x86_64-linux.default;
 
  };
}

{ nixpkgs ? <nixpkgs>
, nixpkgs' ? import nixpkgs {}}: with nixpkgs';

# some comment
stdenv.mkDerivation rec {
  pname = "test";
  version = "0.2.3";
  name = "${pname}-${version}";

  buildInputs = [
    gzip
    bzip2
    python27
  ];
}

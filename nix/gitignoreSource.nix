let
  owner = "hercules-ci";
  repo = "gitignore";
  rev = "c4662e662462e7bf3c2a968483478a665d00e717";
in
  import (builtins.fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    sha256 = "sha256:0fc5bgv9syfcblp23y05kkfnpgh3gssz6vn24frs8dzw39algk2z";
  })

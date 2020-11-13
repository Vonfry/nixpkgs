{ fetchzip, withPEPatterns, lib }:

with lib;
{
  retdec-support =
    let
      version = "2019-03-08"; # make sure to adjust both hashes (once with withPEPatterns=true and once withPEPatterns=false)
    in fetchzip {
      url = "https://github.com/avast/retdec-support/releases/download/${version}/retdec-support_${version}.tar.xz";
      sha256 = if withPEPatterns
               then lib.fakeSha256
               else "0mx3xm2a70fx8vlynkavq8gfd9w5yjcix5rx85444i2s1h6kcd0j";
      stripRoot = false;
      # Removing PE signatures reduces this from 3.8GB -> 642MB (uncompressed)
      extraPostFetch = optionalString (!withPEPatterns) ''
      rm -r "$out/generic/yara_patterns/static-code/pe"
    '';
    } // {
      inherit version; # necessary to check the version against the expected version
    };

  capstone = fetchzip {
    url = "https://github.com/aquynh/capstone/archive/bc8a649b35188786754ea1b0bddd5cb48a039162.zip";
    sha256 = "1qhy2p840qx8rccxc0axd3qj628k7xwsyw3ly4q2lm7vbm7mmjpa";
  };

  googletest = fetchzip {
    url = "https://github.com/google/googletest/archive/90a443f9c2437ca8a682a1ac625eba64e1d74a8a.zip";
    sha256 = "0adgfjm48nl624z77wpk492lddj7f6fm4imdafdchk8rnlqqysky";
  };

  keystone = fetchzip {
    url = "https://github.com/keystone-engine/keystone/archive/d7ba8e378e5284e6384fc9ecd660ed5f6532e922.zip";
    sha256 = "1yzw3v8xvxh1rysh97y0i8y9svzbglx2zbsqjhrfx18vngh0x58f";
  };

  llvm = fetchzip {
    url = "https://github.com/avast/llvm/archive/d17df7fb9a1d585fdfa3643e666506d1bead4443.zip";
    sha256 = "1lx8g5q2z2sl1agwx53mk3c12xwvfnnrgcz7ps3hxcnz2p67ka84";
  };

  openssl = fetchzip {
    url = "https://github.com/openssl/openssl/archive/97ace46e11dba4c4c2b7cb67140b6ec152cfaaf4.zip";
    sha256 = "0rbs6acagzl1zpyv8r4pap85hx22cc6dpfkk1j9y167p26zw57l7";
  };

  yara = fetchzip {
    url = "https://github.com/VirusTotal/yara/archive/b9f925bb4e2b998bd6bb2f2e3cc2087c62fdd5b9.zip";
    sha256 = "0mx3xm2a70fx8vlynkavq8gfd9w5yjcix5rx85444i2s1h6kcd0j";
  };

  yaramod = fetchzip {
    url = "https://github.com/avast/yaramod/archive/57f4ee87372aba7735bbcc1ed870f43faaa8127b.zip";
    sha256 = "1rcsgrsvy8fpmxnr419bsyck18zhpk8mih32glng80h2s47dlzvh";
  };
}

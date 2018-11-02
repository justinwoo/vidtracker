{ pkgs ? import <nixpkgs> {} }:

let
  inputs = {

    aff = pkgs.stdenv.mkDerivation {
      name = "aff";
      version = "v5.0.2";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-aff.git";
        rev = "v5.0.2";
        sha256 = "0jhqaimcg9cglnby0rn5xnrllcjj9mlb5yp6zqpy8b9zpg744v7d";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    aff-promise = pkgs.stdenv.mkDerivation {
      name = "aff-promise";
      version = "v2.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/nwolverson/purescript-aff-promise.git";
        rev = "v2.0.1";
        sha256 = "1zy91qhd0zfxys4bk5ncvvfad6hqpa2ghd4xn8vqp4zj0xr8phwc";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    arraybuffer-types = pkgs.stdenv.mkDerivation {
      name = "arraybuffer-types";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-arraybuffer-types.git";
        rev = "v2.0.0";
        sha256 = "059a8n49yhl46l1b1j2qj4ichzq6dzj29ajkfvw88npzj5w2rshy";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    arrays = pkgs.stdenv.mkDerivation {
      name = "arrays";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-arrays.git";
        rev = "v5.1.0";
        sha256 = "1pcvkgfp8kxk7s1lm28cpc24d0y782n6n6xirkdb09jjh6i62r6s";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    avar = pkgs.stdenv.mkDerivation {
      name = "avar";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-avar.git";
        rev = "v3.0.0";
        sha256 = "14g05jm2xricy5b9vn4k4lhc9lzi5jvpvv85h10s17kn4wwi9igk";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    bifunctors = pkgs.stdenv.mkDerivation {
      name = "bifunctors";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-bifunctors.git";
        rev = "v4.0.0";
        sha256 = "1bdra5fbkraglqrrm484vw8h0wwk48kzkn586v4y7fg106q1q386";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    catenable-lists = pkgs.stdenv.mkDerivation {
      name = "catenable-lists";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-catenable-lists.git";
        rev = "v5.0.0";
        sha256 = "1zhc6mfgzkahrnbrl39vdj6biy75gibmayw8ahj9a23amsggs6df";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    colors = pkgs.stdenv.mkDerivation {
      name = "colors";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/sharkdp/purescript-colors.git";
        rev = "v5.0.0";
        sha256 = "05bkfqllfpkh7nj0nzgd5p387hlpk0x35nam1i6xm3vzma9csj18";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    console = pkgs.stdenv.mkDerivation {
      name = "console";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-console.git";
        rev = "v4.1.0";
        sha256 = "1rc9b53q0l7g37113nspdcxcysg19wfq0l9d84gys8dp3q9n8vbf";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    const = pkgs.stdenv.mkDerivation {
      name = "const";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-const.git";
        rev = "v4.0.0";
        sha256 = "1fzj2zak5a59lxg7vhxsp24hqydhxs8iq89rbl7qm4zcqb0lvw70";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    contravariant = pkgs.stdenv.mkDerivation {
      name = "contravariant";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-contravariant.git";
        rev = "v4.0.0";
        sha256 = "0vvcgfclx236kg4y76nwih787wyqacq8mmx42q64xzl964yrwxkk";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    control = pkgs.stdenv.mkDerivation {
      name = "control";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-control.git";
        rev = "v4.1.0";
        sha256 = "10703zvsnjm5fc74k6wzjsvpsfyc3jci3jxhm7rxf7ymya9z1z53";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    coroutines = pkgs.stdenv.mkDerivation {
      name = "coroutines";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-coroutines.git";
        rev = "v5.0.0";
        sha256 = "1jax7by8kn9fjg00avhziy3n18i3510iwzs3d73ziplanbw4qw4k";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    css = pkgs.stdenv.mkDerivation {
      name = "css";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-css.git";
        rev = "v4.0.0";
        sha256 = "0f6gib6rp20qz08vramw7k6kv2ck315lmshjpii8zmkjb5ya0w55";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    datetime = pkgs.stdenv.mkDerivation {
      name = "datetime";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-datetime.git";
        rev = "v4.0.0";
        sha256 = "0zpjpnsnn95cscc95p9p59g7fvcc4qcl5yjmlrkjln4swi62jggd";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    debug = pkgs.stdenv.mkDerivation {
      name = "debug";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/garyb/purescript-debug.git";
        rev = "v4.0.0";
        sha256 = "0gwjj80akys0h111i74n429fmny992gx0r4rk1n98gqlqm5cmi21";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    distributive = pkgs.stdenv.mkDerivation {
      name = "distributive";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-distributive.git";
        rev = "v4.0.0";
        sha256 = "0zbn0yq1vv7fbbf1lncg80irz0vg7wnw9b9wrzxhdzpbkw4jinsl";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    dom-indexed = pkgs.stdenv.mkDerivation {
      name = "dom-indexed";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-dom-indexed.git";
        rev = "v6.0.0";
        sha256 = "0gbmli54smjwwpwq0pa44vjqsbhhyfvm4c56issrd8f405wgrwn0";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    effect = pkgs.stdenv.mkDerivation {
      name = "effect";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-effect.git";
        rev = "v2.0.0";
        sha256 = "0l46xqz39khf2c779d8mvax1fp2phy5sf8qdn31x67dz389mjr81";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    either = pkgs.stdenv.mkDerivation {
      name = "either";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-either.git";
        rev = "v4.0.0";
        sha256 = "0c72wk4hdcayj1jvwk1i6fny8r9iflxblvng6a265hb9r8gnjnwn";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    enums = pkgs.stdenv.mkDerivation {
      name = "enums";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-enums.git";
        rev = "v4.0.0";
        sha256 = "1g2zns5xsdb9xyv14iwyvg2x39hjpsyvvrkh8gy1pqgzv6frmb18";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    exceptions = pkgs.stdenv.mkDerivation {
      name = "exceptions";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-exceptions.git";
        rev = "v4.0.0";
        sha256 = "17s0rg9k4phivhx9j3l2vqqfdhk61gpj1xfqy8w6zj3rnxj0b2cv";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    exists = pkgs.stdenv.mkDerivation {
      name = "exists";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-exists.git";
        rev = "v4.0.0";
        sha256 = "0bbdbw3jaqyi8dj2d52zvfgx4vl84d8cr6hp43vy8lfjfcbj0wlk";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    foldable-traversable = pkgs.stdenv.mkDerivation {
      name = "foldable-traversable";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foldable-traversable.git";
        rev = "v4.1.0";
        sha256 = "1jcc68nghn1746rn6g6rxhgw1q6jn2mixc3hp3k63hizsjflg5wh";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    foreign = pkgs.stdenv.mkDerivation {
      name = "foreign";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foreign.git";
        rev = "v5.0.0";
        sha256 = "15mz2s4f8crkd721z4df2aag4s0wil6fs07cpcmi7dpnkn7a4nab";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    foreign-object = pkgs.stdenv.mkDerivation {
      name = "foreign-object";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-foreign-object.git";
        rev = "v1.0.0";
        sha256 = "19jz8nqkj2h24r1ay08ba65qqpg4b0a1x31ncsq7jg77ayfhl4ms";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    fork = pkgs.stdenv.mkDerivation {
      name = "fork";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-fork.git";
        rev = "v4.0.0";
        sha256 = "1jygqzyci40c28gw2ygnx8v52hilhajj1bdpn7ndvss4i7yh1lcp";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    free = pkgs.stdenv.mkDerivation {
      name = "free";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-free.git";
        rev = "v5.1.0";
        sha256 = "0f093s5wvbip547fyfxyjqn60z5ay11hs9vzw1h480307vpzq3wm";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    freeap = pkgs.stdenv.mkDerivation {
      name = "freeap";
      version = "v5.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/ethul/purescript-freeap.git";
        rev = "v5.0.1";
        sha256 = "007840vpxa4gz0fvjbahyky2xzz625gzfaiy2wjpb50d9qacsr7y";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    freet = pkgs.stdenv.mkDerivation {
      name = "freet";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-freet.git";
        rev = "v4.0.0";
        sha256 = "1kpgggwimxjvdvhn6s7z4lzg3yw1rqg5f7pyamx9gh232s4va40i";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    functions = pkgs.stdenv.mkDerivation {
      name = "functions";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-functions.git";
        rev = "v4.0.0";
        sha256 = "0675k5kxxwdvsjs6a3is8pwm3hmv0vbcza1b8ls10gymmfz3k3hj";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    functors = pkgs.stdenv.mkDerivation {
      name = "functors";
      version = "v3.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-functors.git";
        rev = "v3.1.0";
        sha256 = "1hdvsznzwl8akkgy0islr48qrqhr3syagggily27lv0d1mjl0rw3";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    gen = pkgs.stdenv.mkDerivation {
      name = "gen";
      version = "v2.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-gen.git";
        rev = "v2.1.0";
        sha256 = "0ddsfb6a23rahkw9d3ymp2sf6d6vxndj73y61cdv74zrlr2nx74p";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    generics-rep = pkgs.stdenv.mkDerivation {
      name = "generics-rep";
      version = "v6.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-generics-rep.git";
        rev = "v6.1.0";
        sha256 = "1d2f03bspgipal4g24x1b8rmx92nk2hwq7k62mix2w4lls2jiizr";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    globals = pkgs.stdenv.mkDerivation {
      name = "globals";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-globals.git";
        rev = "v4.0.0";
        sha256 = "150mc0kv0cb5fkx0szicwczjr54bglmlyaynj2grf1r4gnjg967s";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    gomtang-basic = pkgs.stdenv.mkDerivation {
      name = "gomtang-basic";
      version = "v0.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-gomtang-basic.git";
        rev = "v0.2.0";
        sha256 = "11zp6id0hrq9hmr0w395ldf403p6v6ldz0miisrxmdmpcrcm3j54";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    halogen = pkgs.stdenv.mkDerivation {
      name = "halogen";
      version = "v5.0.0-pre";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-halogen.git";
        rev = "v5.0.0-pre";
        sha256 = "04mhcnbhpbd7nrhycd9g2vgigzcyqaygiqp58i84qsicspj1ny41";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    halogen-css = pkgs.stdenv.mkDerivation {
      name = "halogen-css";
      version = "v8.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-halogen-css.git";
        rev = "v8.0.0";
        sha256 = "1a8sj8ydfnvj3vh2l3f0yyd69y7v4qki1a5m99n0v2aqc1y6nrzl";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    halogen-vdom = pkgs.stdenv.mkDerivation {
      name = "halogen-vdom";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/slamdata/purescript-halogen-vdom.git";
        rev = "v4.0.0";
        sha256 = "0fjhyyvmi4lg2ds5ahxk4il0fq19knj90crw6hz75f0vvid9rily";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    identity = pkgs.stdenv.mkDerivation {
      name = "identity";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-identity.git";
        rev = "v4.0.0";
        sha256 = "0jw61rk4308qmbjxkdb37fdw2r08pzh8z3lg2x29f9l9f7ra5ggw";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    integers = pkgs.stdenv.mkDerivation {
      name = "integers";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-integers.git";
        rev = "v4.0.0";
        sha256 = "17d4bfpnrmbxlc7hhhrvnli70ydaqyr26zgvc9q52a76zgdcb4cf";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    invariant = pkgs.stdenv.mkDerivation {
      name = "invariant";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-invariant.git";
        rev = "v4.1.0";
        sha256 = "1fimpbh3yb7clvqxcdf4yf9im64z0v2s9pbspfacgq5b4vshjas9";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    jajanmen = pkgs.stdenv.mkDerivation {
      name = "jajanmen";
      version = "v0.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-jajanmen.git";
        rev = "v0.2.0";
        sha256 = "1pd0aij1h9y0i3pzz8x8liv80fxlm27yiybrs93xqh275a706km3";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    js-date = pkgs.stdenv.mkDerivation {
      name = "js-date";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-js-date.git";
        rev = "v6.0.0";
        sha256 = "19qyzbr4a1ca8znbd8gcbz0a801f5p1v238ky3408gdx4ba32zjd";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    js-timers = pkgs.stdenv.mkDerivation {
      name = "js-timers";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-js-timers.git";
        rev = "v4.0.1";
        sha256 = "1a8092sli7zqw1wflibhjza1ww21dxl7x7r602iazzwh2g70v4dg";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    lazy = pkgs.stdenv.mkDerivation {
      name = "lazy";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lazy.git";
        rev = "v4.0.0";
        sha256 = "156q89l4nvvn14imbhp6xvvm82q7kqh1pyndmldmnkhiqyr84vqv";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    lcg = pkgs.stdenv.mkDerivation {
      name = "lcg";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lcg.git";
        rev = "v2.0.0";
        sha256 = "1851cq2g84jzjbvbmncbivbhaqzj9zv5ni3gs14k04nmx2brxmvj";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    lenient-html-parser = pkgs.stdenv.mkDerivation {
      name = "lenient-html-parser";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-lenient-html-parser.git";
        rev = "v4.0.0";
        sha256 = "19i4vcj93nz400yiyl4rj190alp5yv9lxm4vw83a5nrk32pll5xi";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    lists = pkgs.stdenv.mkDerivation {
      name = "lists";
      version = "v5.3.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-lists.git";
        rev = "v5.3.0";
        sha256 = "14z4pmw76h3rj6mqwkxny91nqrk5rj5drsl4za2sng83bkj9fj4k";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    makkori = pkgs.stdenv.mkDerivation {
      name = "makkori";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-makkori.git";
        rev = "v1.0.0";
        sha256 = "09i5j2xqp6rjmljc6kbn3h1ybw1j3dfwdl6bxzr0wqk3y14wpdlq";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    math = pkgs.stdenv.mkDerivation {
      name = "math";
      version = "v2.1.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-math.git";
        rev = "v2.1.1";
        sha256 = "1msmy9w7y6fij62sdc55w68gpwkhm6lhgc8qjisjk4sxx1wdg1rr";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    maybe = pkgs.stdenv.mkDerivation {
      name = "maybe";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-maybe.git";
        rev = "v4.0.0";
        sha256 = "06mm4a6lbp5by14vms3lyhqp64211lwnq1dqbaazvdp0afykx1z5";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    media-types = pkgs.stdenv.mkDerivation {
      name = "media-types";
      version = "v4.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-media-types.git";
        rev = "v4.0.1";
        sha256 = "0ykwmxrhmwfy6c5mxjxa43xdf5xqakrqyvr5fn986yad50gjqj75";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    milkis = pkgs.stdenv.mkDerivation {
      name = "milkis";
      version = "v6.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-milkis.git";
        rev = "v6.0.1";
        sha256 = "1x12h3bsc09373j3z87cv5w3xs8by17k8xnjsf8gyx51pbfbfsf3";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    naporitan = pkgs.stdenv.mkDerivation {
      name = "naporitan";
      version = "v0.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-naporitan.git";
        rev = "v0.2.0";
        sha256 = "0g9aav1bzrkda6zv8d77hy5gf0jjm7x026rfbbqsrm6flfi8k85n";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    newtype = pkgs.stdenv.mkDerivation {
      name = "newtype";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-newtype.git";
        rev = "v3.0.0";
        sha256 = "0qvk9p41miy806b05b4ikbr3if0fcyc35gfrwd2mflcxxp46011c";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-buffer = pkgs.stdenv.mkDerivation {
      name = "node-buffer";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-buffer.git";
        rev = "v5.0.0";
        sha256 = "0ih2y29srdxgn526fw2v1y95hpivjil44vkl93w6nrqsymki36y0";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-child-process = pkgs.stdenv.mkDerivation {
      name = "node-child-process";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-child-process.git";
        rev = "v5.0.0";
        sha256 = "0igspvgsqabwrgync4znn7gg89xl4lck9hdqfscawj8fj7rg960d";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-fs = pkgs.stdenv.mkDerivation {
      name = "node-fs";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-fs.git";
        rev = "v5.0.0";
        sha256 = "1hkg8j4zkyq71g2bn3vpfqb8x49rdd9k0ayv3zf6l8k80gp3qigx";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-fs-aff = pkgs.stdenv.mkDerivation {
      name = "node-fs-aff";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-fs-aff.git";
        rev = "v6.0.0";
        sha256 = "0vjc9zag7y20yxhhv45hrwv9fbpmp0szv40vaxl5x8mnd5wv28x7";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-http = pkgs.stdenv.mkDerivation {
      name = "node-http";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-http.git";
        rev = "v5.0.0";
        sha256 = "1pwh3alx32gcih2w050pac4yy9l4q6zxar8i7yri3j23nfg6lcpi";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-path = pkgs.stdenv.mkDerivation {
      name = "node-path";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-path.git";
        rev = "v3.0.0";
        sha256 = "0j1ni52m62dpcrfakl1ik131i31bkg91yv0p1c40sdw0f59fzf6x";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-process = pkgs.stdenv.mkDerivation {
      name = "node-process";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-process";
        rev = "v6.0.0";
        sha256 = "19p4ylxn9cfa7drxrl4gzsfp52jwgm67n5k7rs9lazkp1lvmk0jh";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-sqlite3 = pkgs.stdenv.mkDerivation {
      name = "node-sqlite3";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-node-sqlite3";
        rev = "v5.0.0";
        sha256 = "0shpp6w4wp81lh49p38yv5rrxy1n4b9wkiywdg8dqf843946bjhp";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-streams = pkgs.stdenv.mkDerivation {
      name = "node-streams";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-streams.git";
        rev = "v4.0.0";
        sha256 = "098wdq0rj4nkc470fwmiaars7vxac9n1dh4d82jrji3m77n473da";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    node-url = pkgs.stdenv.mkDerivation {
      name = "node-url";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-node-url.git";
        rev = "v4.0.0";
        sha256 = "0qbpdz62psy7hb34hw5rw2x1pq7yhd214ysza0xh46c3nlp0ib9y";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    nonempty = pkgs.stdenv.mkDerivation {
      name = "nonempty";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-nonempty.git";
        rev = "v5.0.0";
        sha256 = "1vz174sg32cqrp52nwb2vz9frrzmdwzzlgl4vc2cm5wlf2anirxj";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    now = pkgs.stdenv.mkDerivation {
      name = "now";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-now.git";
        rev = "v4.0.0";
        sha256 = "18h5pif2dy4r7w1xg2zw4mvdqlar9xqp3rawkiavmsc946ncf3zs";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    nullable = pkgs.stdenv.mkDerivation {
      name = "nullable";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-nullable.git";
        rev = "v4.1.0";
        sha256 = "1m5j2v0zp9s2349khmvxz98fsfxxn3pzxjiv87rnxd72rsray68v";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    numbers = pkgs.stdenv.mkDerivation {
      name = "numbers";
      version = "v6.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/sharkdp/purescript-numbers.git";
        rev = "v6.0.0";
        sha256 = "1kzdg69llbmrccmgwrpfiq6hr6jdg2zl40qgzj10nzbhgqrbv2y1";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    options = pkgs.stdenv.mkDerivation {
      name = "options";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-options.git";
        rev = "v4.0.0";
        sha256 = "1yfsx3fxwqkcb7q6kifbsrx8wnr8j6gg5nm3ybgiwkbdk6s1j7v2";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    ordered-collections = pkgs.stdenv.mkDerivation {
      name = "ordered-collections";
      version = "v1.4.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-ordered-collections.git";
        rev = "v1.4.0";
        sha256 = "0kh1hxs5lqmdzjf8zs7i8val9l5z67l7g10rgbnkln2j54mym3cf";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    orders = pkgs.stdenv.mkDerivation {
      name = "orders";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-orders.git";
        rev = "v4.0.0";
        sha256 = "13p1sm4dxkmxhld9x5qqg62iiajjb3qpzs66c1r24y5fs4zsfry4";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    parallel = pkgs.stdenv.mkDerivation {
      name = "parallel";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-parallel.git";
        rev = "v4.0.0";
        sha256 = "1d5bnagabw2k8yxywkygwrpblb2ggqh2fhpqfrx2sj1y53x332hg";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    partial = pkgs.stdenv.mkDerivation {
      name = "partial";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-partial.git";
        rev = "v2.0.0";
        sha256 = "0nw5989ydin2d12b97ch4pdynxkq91xpj7yym5gpd5fpbgy36mdi";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    posix-types = pkgs.stdenv.mkDerivation {
      name = "posix-types";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-node/purescript-posix-types";
        rev = "v4.0.0";
        sha256 = "0xvxjvxr7n4dv53p8w5qfmgx37cga5bp2rjhkbfvj89rb74vm3i1";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    prelude = pkgs.stdenv.mkDerivation {
      name = "prelude";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-prelude.git";
        rev = "v4.1.0";
        sha256 = "1pwqhsba4xyywfflma5rfqzqac1vmybwq7p3wkm4wsackvbn34h5";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    prettier = pkgs.stdenv.mkDerivation {
      name = "prettier";
      version = "v0.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/gcanti/purescript-prettier.git";
        rev = "v0.2.0";
        sha256 = "17fkkdiiwd4ydqg606ragawxlf8kkl9nnizpsciginzw85jzpy5s";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    profunctor = pkgs.stdenv.mkDerivation {
      name = "profunctor";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-profunctor.git";
        rev = "v4.0.0";
        sha256 = "1v4kvmhmiwznd4lswp9339h64pgv5zvd3vm1q7gzj70767a3941i";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    profunctor-lenses = pkgs.stdenv.mkDerivation {
      name = "profunctor-lenses";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-profunctor-lenses.git";
        rev = "v5.0.0";
        sha256 = "10by0cbz68mqbdwhbrzp1lvyyg72lp636lpb4vczq0fxvw3q5v9x";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    proxy = pkgs.stdenv.mkDerivation {
      name = "proxy";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-proxy.git";
        rev = "v3.0.0";
        sha256 = "0rqf25b1n9p5sgx7gdsxwrfv9rb3sqxgqmqpp5kdm30lfk7snz24";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    quickcheck = pkgs.stdenv.mkDerivation {
      name = "quickcheck";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-quickcheck.git";
        rev = "v5.0.0";
        sha256 = "18j0qg1xz7vzr7lscg2nn59i2cn3f3cf0pazbi3ka7nf4i1k1b1s";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    random = pkgs.stdenv.mkDerivation {
      name = "random";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-random.git";
        rev = "v4.0.0";
        sha256 = "0k37v7z529adx6ypxj0xjyfrz45qia6p0vki2wpvi8lik7c698gf";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    record = pkgs.stdenv.mkDerivation {
      name = "record";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-record.git";
        rev = "v1.0.0";
        sha256 = "1vx6qlcg8x8cij3jsf52gqnd1dvam36pw83x4sad1ddir2s5h0i8";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    record-format = pkgs.stdenv.mkDerivation {
      name = "record-format";
      version = "v0.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/kcsongor/purescript-record-format.git";
        rev = "v0.1.0";
        sha256 = "1bw00l68azyad6b33iqhfjmm0n1j58h1ljli9772ybwhh8slsh5h";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    refs = pkgs.stdenv.mkDerivation {
      name = "refs";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-refs.git";
        rev = "v4.1.0";
        sha256 = "08161iy1xbafzblv033v84156azpcqkiw9v9d6gliphrq5fm17gm";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    shoronpo = pkgs.stdenv.mkDerivation {
      name = "shoronpo";
      version = "v0.3.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-shoronpo.git";
        rev = "v0.3.0";
        sha256 = "0vyzs7yzc8dxvci2y6qzbwa82ag8big5qwiffx73fzvk52k5hfy6";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    simple-json = pkgs.stdenv.mkDerivation {
      name = "simple-json";
      version = "v4.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-simple-json.git";
        rev = "v4.2.0";
        sha256 = "1iyiy2hl9dsc9y34n6izn4qpj72wpv9a45lj6k6br3vlmm2f00z1";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    st = pkgs.stdenv.mkDerivation {
      name = "st";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-st.git";
        rev = "v4.0.0";
        sha256 = "0m2jkb9dmpbr8s1c20l7sm2q11y5rx8gqfiyspnyhq5apzkknjr0";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    string-parsers = pkgs.stdenv.mkDerivation {
      name = "string-parsers";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/paf31/purescript-string-parsers.git";
        rev = "v5.0.0";
        sha256 = "1lykswyd3icv9lx4r096lxfd244i0pzj2f4fpm604czzlfw6bspv";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    strings = pkgs.stdenv.mkDerivation {
      name = "strings";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-strings.git";
        rev = "v4.0.0";
        sha256 = "0i7i3irhx1l9abprll95fi374gp49qka0vbhk80cbnhmx1v5px5k";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    sunde = pkgs.stdenv.mkDerivation {
      name = "sunde";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-sunde.git";
        rev = "v1.0.0";
        sha256 = "1s62al6ark10hqc6kdz3nqarblpa6fmdy5kb3bjlxsa8nib436pa";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    tailrec = pkgs.stdenv.mkDerivation {
      name = "tailrec";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-tailrec.git";
        rev = "v4.0.0";
        sha256 = "0z7k80nl8dgv8mc2w8xsl2n0637bd1l8ppxak8kaifgjjwa81hx3";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    test-unit = pkgs.stdenv.mkDerivation {
      name = "test-unit";
      version = "v14.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/bodil/purescript-test-unit.git";
        rev = "v14.0.0";
        sha256 = "0iyqdrhb0n5qpck8cprl68034ivywmphbw5wr7zy1jlyj9450nwh";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    these = pkgs.stdenv.mkDerivation {
      name = "these";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-these.git";
        rev = "v4.0.0";
        sha256 = "0ywwpbcz1d0pdi3f9h9kla52vq1if8zwdz7jq7lqz5s8zj8kyg5r";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    toppokki = pkgs.stdenv.mkDerivation {
      name = "toppokki";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-toppokki.git";
        rev = "v1.0.0";
        sha256 = "00blhsk7j3xhvp16kj2qlmzd01g1v8nn154gr21l4krxrdb5553y";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    tortellini = pkgs.stdenv.mkDerivation {
      name = "tortellini";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/justinwoo/purescript-tortellini.git";
        rev = "v3.0.0";
        sha256 = "0gpgr4f2l5rn1zy0iqy42gpll01abb11rd7s9gx9yzcj43l1fsgb";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    transformers = pkgs.stdenv.mkDerivation {
      name = "transformers";
      version = "v4.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-transformers.git";
        rev = "v4.1.0";
        sha256 = "1aazy1zk66lng8w0gjx2l7sqfr968gmibdxi4kd93zb7bw5vldvn";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    tuples = pkgs.stdenv.mkDerivation {
      name = "tuples";
      version = "v5.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-tuples.git";
        rev = "v5.0.0";
        sha256 = "0vlhv4l2a2vb6rh5zfsjhbv7hy4vz9fa5p1ns0rk54xd2pzzlvgd";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    type-equality = pkgs.stdenv.mkDerivation {
      name = "type-equality";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-type-equality.git";
        rev = "v3.0.0";
        sha256 = "1b7szyca5s96gaawvgwrw7fa8r7gqsdff7xhz3vvngnylv2scl3w";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    typelevel-prelude = pkgs.stdenv.mkDerivation {
      name = "typelevel-prelude";
      version = "v3.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-typelevel-prelude.git";
        rev = "v3.0.0";
        sha256 = "0dn95n9jnk2ilw38cf9p8p6q3xad6ck1rq3r4jba92kmk4ql665r";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    unfoldable = pkgs.stdenv.mkDerivation {
      name = "unfoldable";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-unfoldable.git";
        rev = "v4.0.0";
        sha256 = "077vl30j3pxr3zw6cw7wd0vi22j92j8va15r26rn53wzbzcgr41j";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    unsafe-coerce = pkgs.stdenv.mkDerivation {
      name = "unsafe-coerce";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-unsafe-coerce.git";
        rev = "v4.0.0";
        sha256 = "0k9255mk2mz6xjb11pwkgfcblmmyvr86ig5kr92jwy95xim09zip";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    unsafe-reference = pkgs.stdenv.mkDerivation {
      name = "unsafe-reference";
      version = "v3.0.1";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-contrib/purescript-unsafe-reference";
        rev = "v3.0.1";
        sha256 = "0q758dz59qz0li4s3w1qcg921xp5i5rh6i1l611iv7rr8cbj11al";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    validation = pkgs.stdenv.mkDerivation {
      name = "validation";
      version = "v4.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript/purescript-validation.git";
        rev = "v4.0.0";
        sha256 = "1y5i1jpr6jn1yjjpi4l5mk7jyd5v21fym892s0y5bdn4nj7i465l";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    variant = pkgs.stdenv.mkDerivation {
      name = "variant";
      version = "v5.1.0";
      src = pkgs.fetchgit {
        url = "https://github.com/natefaubion/purescript-variant.git";
        rev = "v5.1.0";
        sha256 = "045qkiv9j0pbdffyzn4r3gr75ahl7h9kxmib6q1bpds1c490pcg6";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-clipboard = pkgs.stdenv.mkDerivation {
      name = "web-clipboard";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-clipboard.git";
        rev = "v1.0.0";
        sha256 = "0zbhrm4ck8a8wf1knpmxccfyws07frld8i70wnkwjfan1pgdji63";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-dom = pkgs.stdenv.mkDerivation {
      name = "web-dom";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-dom.git";
        rev = "v1.0.0";
        sha256 = "070ybc5xzpsh75p618ll9j0lzxqgvc4cl5cdrn71v0vagmyhgm8h";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-events = pkgs.stdenv.mkDerivation {
      name = "web-events";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-events.git";
        rev = "v1.0.0";
        sha256 = "1j6gkap9ap1ik7aiiyxwdmvjfhfrw73dq5hi9a671k5prlma7v3k";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-file = pkgs.stdenv.mkDerivation {
      name = "web-file";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-file.git";
        rev = "v1.0.0";
        sha256 = "14kzwrwbfacxr4krfriy8qx13wnhiydrgs6hp59a7bmx58w0ifks";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-html = pkgs.stdenv.mkDerivation {
      name = "web-html";
      version = "v1.2.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-html.git";
        rev = "v1.2.0";
        sha256 = "1y0g2ginqmfng058c99dm9n25vhz3g76d4l2v7yyrln9w04m4mvx";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-storage = pkgs.stdenv.mkDerivation {
      name = "web-storage";
      version = "v2.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-storage.git";
        rev = "v2.0.0";
        sha256 = "1lr4lswsp62kl21rl6jb377a0sya105sfimkxm3y22grx88l6vj1";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-touchevents = pkgs.stdenv.mkDerivation {
      name = "web-touchevents";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-touchevents.git";
        rev = "v1.0.0";
        sha256 = "1avg7jljd1j7d0b7k7fbqxi90aa01awp9x17v53mynzg6gyvc8zb";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };

    web-uievents = pkgs.stdenv.mkDerivation {
      name = "web-uievents";
      version = "v1.0.0";
      src = pkgs.fetchgit {
        url = "https://github.com/purescript-web/purescript-web-uievents.git";
        rev = "v1.0.0";
        sha256 = "1683ysabddnhsjiy70axpslagi2cp2ab3qd0r1qjx6ab2zjd6kc0";
      };
      dontInstall = true;
      buildPhase = "cp --no-preserve=mode,ownership,timestamp -r $src $out";
    };
};

in {
  inherit inputs;

  set = "local";
  source = "";
}

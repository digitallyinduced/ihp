{ localPkgs ? import <nixpkgs> {}
, imagePkgs ? import <nixpkgs> { system = "x86_64-linux"; }
, ihpApp ? import ./../default.nix { additionalNixpkgsOptions = { system = "x86_64-linux"; }; }
}:
localPkgs.dockerTools.buildImage {
  name = "app";
  config = {
    Cmd = [ "${ihpApp}/bin/RunDevServer" ];
    WorkingDir = "/home/app";
    ExposedPorts = {
      "8000/tcp" = {};
      "8001/tcp" = {};
    };
    User = "app";
  };
  extraCommands = ''
    mkdir -p home/app/build
    mkdir -p home/app/Application home/app/Migration
    touch home/app/Application/Fixtures.sql
    cp ${ihpApp}/lib/Schema.sql home/app/Application/Schema.sql
    chmod -R 777 home/app
    ln -s ${ihpApp}/lib/build/ihp-lib home/app/build/ihp-lib
    
  '';
  contents = [imagePkgs.cacert];
  fromImage =
    let
      nonRootShadowSetup = { user, uid, gid ? uid }: with imagePkgs; [
        (
        writeTextDir "etc/shadow" ''
          root:!x:::::::
          ${user}:!:::::::
        ''
        )
        (
        writeTextDir "etc/passwd" ''
          root:x:0:0::/root:${runtimeShell}
          ${user}:x:${toString uid}:${toString gid}::/home/${user}:
        ''
        )
        (
        writeTextDir "etc/group" ''
          root:x:0:
          ${user}:x:${toString gid}:
        ''
        )
        (
        writeTextDir "etc/gshadow" ''
          root:x::
          ${user}:x::
        ''
        )
      ];
    in
      localPkgs.dockerTools.buildImage {
        name = "bash";
        tag = "latest";
        contents = [ imagePkgs.bashInteractive ] ++ nonRootShadowSetup { uid = 1337; user = "app"; };
      };
}
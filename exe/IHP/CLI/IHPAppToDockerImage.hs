{-|
Module: IHP.CLI.IHPAppToDockerImage
Description: Provides the @ihp-app-to-docker-image@ command which generates a docker image out of the current IHP app
Copyright: (c) digitally induced GmbH, 2021
-}
module Main where

import IHP.Prelude
import qualified System.Directory as Directory
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import qualified System.Process as Process
import qualified System.Info as System
import qualified System.Exit as Exit
import qualified Control.Exception as Exception

main :: IO ()
main = do
    ensureIsInAppDirectory
    ensureDockerRemoteBuilder
    Directory.createDirectoryIfMissing True "build"
    createDockerNix
    createAppNix

    removeFileIfExists "docker.tar.gz"    
    exitCode <- Process.system "NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nix-build build/docker.nix -o docker.tar.gz"

    let isSuccess = exitCode == Exit.ExitSuccess
    if isSuccess
        then putStrLn "✅ The docker image is at 'docker.tar.gz'"
        else Exit.exitFailure

createDockerNix :: IO ()
createDockerNix = Text.writeFile "build/docker.nix" dockerNixContent

createAppNix :: IO ()
createAppNix = do
    appNixContent <- getAppNixContent
    Text.writeFile "build/app.nix" appNixContent

getAppNixContent :: IO Text
getAppNixContent = do
    defaultNix <- Text.readFile "default.nix"
    let patchAdditionalNixpkgsOptions = Text.replace "projectPath" "additionalNixpkgsOptions = additionalNixpkgsOptions; projectPath"
    let patchPaths = Text.replace "./" "./../"
    let appNix = "{ additionalNixpkgsOptions ? {} }:\n" <> (defaultNix |> patchAdditionalNixpkgsOptions |> patchPaths)
    pure appNix

ensureDockerRemoteBuilder :: IO ()
ensureDockerRemoteBuilder = when isMacOS do
    ensureDockerIsRunning

    remoteBuilder <- doesRemoteBuilderExists
    unless remoteBuilder initRemoteBuilder
    when remoteBuilder ensureRemoteBuilderIsStarted

-- | If docker is not running this will throw an exception
--
-- Only used on macOS
ensureDockerIsRunning :: IO ()
ensureDockerIsRunning = do
    let config = (Process.shell "docker ps") { Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
    (_, Just stdoutHandle, Just stderrHandle, processHandle) <- Process.createProcess config

    stdout <- Text.hGetContents stdoutHandle
    stderr <- Text.hGetContents stderrHandle

    Process.waitForProcess processHandle

    let failedToConnect = "Is the docker daemon running" `Text.isInfixOf` stderr
    when failedToConnect (error "The docker daemon is not running. Please start docker and re-run this command again.")

    pure ()

-- | Returns True when a docker container with the name 'nix-docker' exists. The container could still be in a stopped state.
--
-- Only used on macOS
doesRemoteBuilderExists :: IO Bool
doesRemoteBuilderExists = do
    let config = (Process.shell "docker ps -a -f name=nix-docker -q|wc -l") { Process.std_out = Process.CreatePipe }
    (_, Just stdoutHandle, _, processHandle) <- Process.createProcess config

    stdout <- Text.hGetContents stdoutHandle

    Process.waitForProcess processHandle

    let count :: Int = stdout
            |> Text.strip
            |> textToInt
            |> fromMaybe 0
    pure (count == 1)

ensureRemoteBuilderIsStarted :: IO ()
ensureRemoteBuilderIsStarted = do
    Process.callCommand "docker start nix-docker"
    pure ()

-- | Set's up a 'nix-docker' container to use a a nix remote builder on macOS.
--
-- The big trouble on macOS is that we need to make linux binaries (docker on macOS is run inside a linux VM).
-- This requires us to have a linux machine as a nix remote builder. Luckily we can just create a docker container
-- with nix installed and use that as our remote linux builder.
--
-- This implementation is based on https://github.com/LnL7/nix-docker
--
initRemoteBuilder :: IO ()
initRemoteBuilder = initRemoteBuilder' `Exception.onException` cleanupTempResources
    where
        initRemoteBuilder' = do
            Process.callCommand "docker run --restart always --name nix-docker -d -p 3022:22 lnl7/nix:ssh"

            isMultiUserMode <- detectNixMultiUserMode
            let isSingleUserMode = not isMultiUserMode

            putStrLn ("Nix Multi User Mode: " <> tshow isMultiUserMode)

            Process.callCommand "ssh-keygen -t rsa -b 2048 -N \"\" -f docker_rsa"
            Process.callCommand "chmod 600 docker_rsa"

            when isSingleUserMode do
                Process.callCommand "cp docker_rsa ~/.ssh/docker_rsa"
                Process.callCommand "echo \"\n# Added by ihp-app-to-docker-image\nHost nix-docker\n  User root\n  HostName 127.0.0.1\n  Port 3022\n  IdentityFile ~/.ssh/docker_rsa\n\" >> ~/.ssh/config"

            when isMultiUserMode do
                Process.callCommand "sudo mkdir -p /etc/nix"
                Process.callCommand "scp docker_rsa.pub nix-docker:/root/.ssh/authorized_keys"
                Process.callCommand "chmod 600 docker_rsa"
                Process.callCommand "sudo cp docker_rsa /etc/nix/docker_rsa"
                Process.callCommand "echo \"\n# Added by ihp-app-to-docker-image\nHost nix-docker\n  User root\n  HostName 127.0.0.1\n  Port 3022\n  IdentityFile /etc/nix/docker_rsa\n\" >> /var/root/.ssh/config"

            Process.callCommand "openssl genrsa -out build/signing-key.sec 2048"
            Process.callCommand "openssl rsa -in build/signing-key.sec -pubout > build/signing-key.pub"

            putStrLn "ℹ️  We need to connect your local nix builder with the 'nix-docker' docker container (which we just created)"
            putStrLn "    For doing that we'll need sudo permissions."

            Process.callCommand "sudo cp build/signing-key.sec /etc/nix/signing-key.sec"
            Process.callCommand "sudo cp build/signing-key.pub /etc/nix/signing-key.pub"

            Process.callCommand "sudo chmod 600 /etc/nix/signing-key.sec"

            Process.callCommand "ssh nix-docker mkdir -p /etc/nix"
            Process.callCommand "scp build/signing-key.sec nix-docker:/etc/nix/signing-key.sec"

            Text.writeFile "build/remote-build-env" remoteBuildEnvFileContent
            Text.writeFile "build/machines" machinesFileContent

            Process.callCommand "sudo cp build/remote-build-env /etc/nix/"
            Process.callCommand "sudo cp build/machines /etc/nix/"

        cleanupTempResources = do
            removeFileIfExists "build/docker_rsa"
            removeFileIfExists "~/.ssh/docker_rsa"
            removeFileIfExists "build/signing-key.sec"
            removeFileIfExists "build/signing-key.pub"

            dockerExists <- doesRemoteBuilderExists
            when dockerExists do
                Process.callCommand "docker stop nix-docker && docker rm nix-docker"

removeFileIfExists path = do
    exists <- Directory.doesFileExist path
    when exists (Directory.removeFile path)
                

-- | https://github.com/LnL7/nix-docker/blob/master/ssh/machines
machinesFileContent :: Text
machinesFileContent = "nix-docker x86_64-linux /etc/nix/docker_rsa 4\n"

-- | https://github.com/LnL7/nix-docker/blob/master/ssh/remote-build-env
remoteBuildEnvFileContent :: Text
remoteBuildEnvFileContent =
       "#!/usr/bin/env bash\n"
    <> "\n"
    <> "mkdir -p /tmp/nix/current-load\n"
    <> "chmod a+rwX /tmp/nix/current-load\n"
    <> "\n"
    <> "export NIX_BUILD_HOOK=\"${HOME}/.nix-profile/libexec/nix/build-remote.pl\"\n"
    <> "export NIX_REMOTE_SYSTEMS=\"/etc/nix/remote-systems.conf\"\n"
    <> "export NIX_CURRENT_LOAD=\"/tmp/nix/current-load\"\n"

-- | Returns True if nix is running in multi user mode
--
-- Only works on macOS
detectNixMultiUserMode :: IO Bool
detectNixMultiUserMode = do
    let config = (Process.shell "grep nixbld /etc/group|wc -l") { Process.std_out = Process.CreatePipe }
    (_, Just stdoutHandle, _, processHandle) <- Process.createProcess config

    stdout <- Text.hGetContents stdoutHandle

    Process.waitForProcess processHandle

    let count :: Int = stdout
            |> Text.strip
            |> textToInt
            |> fromMaybe 0
    pure (count == 1)

dockerNixContent :: Text
dockerNixContent =
       "{ localPkgs ? import <nixpkgs> {}\n"
    <> ", imagePkgs ? import <nixpkgs> { system = \"x86_64-linux\"; }\n"
    <> ", ihpApp ? import ./app.nix { additionalNixpkgsOptions = { system = \"x86_64-linux\"; }; }\n"
    <> "}:\n"
    <> "\n"
    <> "localPkgs.dockerTools.buildImage {\n"
    <> "  name = \"app\";\n"
    <> "  config = {\n"
    <> "    Cmd = [ \"${ihpApp}/bin/RunProdServer\" ];\n"
    <> "    WorkingDir = \"${ihpApp}/lib\";\n"
    <> "    ExposedPorts = {\n"
    <> "      \"8000\" = {};\n"
    <> "    };\n"
    <> "  };\n"
    <> "}\n"

isMacOS :: Bool
isMacOS = System.os == "darwin"

ensureIsInAppDirectory :: IO ()
ensureIsInAppDirectory = do
    mainHsExists <- Directory.doesFileExist "Main.hs"
    unless mainHsExists (fail "You have to be in a project directory to run the generator")

# Deployment

```toc

```

## Deploying with `deploy-to-nixos`

IHP comes with a standard command called `deploy-to-nixos`. This tool is a little wrapper around `nixos-rebuild` and allows you to deploy IHP apps to a NixOS server. With `deploy-to-nixos` you can manage your servers in a fully declarative way and keep the full configuration in your git repository.

AWS EC2 is a good choice for deploying IHP in a professional setup.

### AWS infrastructure preparation

#### Creating infrastructure with Terraform

The EC2 instance, RDS database, VPS, subnets, security groups, etc, can be setup automatically using [Terraform](https://www.terraform.io/).

1. Install terraform
2. Setup AWS credentials in `.aws/config` and `.aws/credentials`
3. Copy the files from the IaC/aws folder from [the branch IaC-aws in ihp-boilerplate](https://github.com/digitallyinduced/ihp-boilerplate/tree/IaC-aws) to you IHP project repo. Run the init command from the IaC/aws folder:
   ```
   terraform init
   ```
4. Create the file `terraform.tfvars` with the following content:
   ```
   prefix = "Project prefix for the resource names"
   region = "AWS Region to deploy to"
   az_1 = "Availability Zone 1"
   az_2 = "Availability Zone 2"
   key_name = "The key name of the SSH key-pair"
   db_password = "The password for the RDS database"
   ```
   - The two AZs are needed to setup the RDS database.
   - The SSH key-pair should be created in the AWS web interface.
5. Run:
   ```
   terraform apply
   ```
6. Important data like the RDS endpoint and the EC2 instance URL is written to the file `db_info.txt`

Now the NixOS instance and Postgres database is setup and an SSH conncetion can be established to it.

#### Creating a new EC2 Instance

Start a new EC2 instance and use the official NixOS AMI `NixOS-23.05.426.afc48694f2a-x86_64-linux`. You can find the latest NixOS AMI at https://nixos.org/download#nixos-amazon

Example steps:
 - Visit [EC2 creation page](https://eu-west-1.console.aws.amazon.com/ec2/home?region=eu-west-1#LaunchInstances:) in your desired region.
 - Select AMI by name, it will appear under "Community AMIs" after searching by name (there can be a slight delay before the result appears as it searches in all community AMIs).
 - Select at least a `t3a.small` instance size to have enough RAM for the compilation. For a real-world application, chances are that you need `t3a.medium` to successfully compile it.
 - Specify a generous root disk volume. By nature NixOS can consume lots of disk space as you trial-and-error your application deployment. As a minimum, we advise 60 GiB.
 - Under `Network settings`, allow SSH traffic from your IP address only, allow HTTPS and HTTP traffic from the internet. Due to the certificate validation for Let's Encrypt, even if your application does not need to have it, allow HTTP too.
 - Make sure to attach SSH keys to the instance at creation time, that is available locally, so you can SSH to the EC2 instance without password later.
   - Either before creating the EC2 instance, you import your existing keypair to [EC2 Key Pairs](https://us-east-1.console.aws.amazon.com/ec2/home?region=eu-west-1#ImportKeyPair:), then you should select it at the EC2 creation page.
   - Or let AWS create one on-the-fly: ![image](https://github.com/digitallyinduced/ihp/assets/114076/317b022a-ad6e-43ae-931d-8710db0b711c) . Afterwards, you will be able to download the private key file, later on it is referred as `ihp-app.pem` in this documentation.

#### (Optional) Creating an RDS Instance

For production systems, it is advised to use a fully managed PostgreSQL instance, it can be multi-region, fault tolerant, but most of all,
daily backups happen automatically with configurable retention.

To switch from the local PostgreSQL instance to a managed one (you can do it after or before the initial deployment), you can execute the following steps:
 - Visit [RDS creation page](https://us-east-1.console.aws.amazon.com/rds/home?region=eu-west-1#launch-dbinstance:) in your desired region.
 - Select PostgreSQL as the Engine Type.
 - Select a compatible Engine Version, there are good chances that the very last version will fit.
 - At Templates, Choose `Free Tier` for any non-live environments, `Production` for the live environment.
 - (Optional) Choose `Auto generate password` for having a secure master password.
 - Choose `Connect to an EC2 compute resource` and select your already existing EC2 instance.
 - Then you can `Create database`. This process is slow, check back in 10 minutes or so afterward. Note down the auto-generated password.
 - Edit your `flake.nix`, under `flake.nixosConfigurations."ihp-app".services.ihp`, you can specify the database URL like: `databaseUrl = lib.mkForce "postgresql://postgres:YOUR-PASSWORD@YOUR-HOSTNAME.amatonaws.com/postgres";`. You can find the proper hostname after the initialization is complete, on the RDS instance detail page.
 - `pg_dump --no-owner --no-acl` your existing local database on the EC2 instance directly, and then, you can load it to the newly created instance via `pgsql`. `deploy-to-nixos` won't populate the initial schema at an existing remote database, that's why dumping, `scp d`ing and loading it via `psql` is necessary.

#### (Optional) Creating an S3 bucket

If your application needs to store files, on AWS, those should use an S3 bucket for that.

Infrastructure-side preparation:
 - Visit the [S3 creation page](https://s3.console.aws.amazon.com/s3/bucket/create?region=eu-west-1) and create a bucket in the same region..If objects should or should not be public, it's up to the application's business requirements. The S3 [ARN](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference-arns.html) from the S3 details page should be noted down.
 - Create an new IAM user for the S3 access. Create an [AWS access key](https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_access-keys.html) for that IAM user.
 - For that user, attach a policy that allows access to the bucket, for example:
```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "VisualEditor1",
            "Effect": "Allow",
            "Action": "s3:*",
            "Resource": [
                "YOUR-BUCKET-ARN",
                "YOUR-BUCKET-ARN/*"
            ]
        }
    ]
}
```
 - See the [Storage guide](https://ihp.digitallyinduced.com/Guide/file-storage.html#s3) on how to use the access key.

 If your application requires so, make the S3 bucket publicly available.
  - Go to https://s3.console.aws.amazon.com/s3/buckets/YOUR-BUCKET?region=eu-west-1&bucketType=general&tab=permissions (permissions tab of the S3 bucket)
  - Set `Block all public access` to entirely off.
  - Set a bucket policy like this:
```json
{
    "Version": "2012-10-17",
    "Statement": [
        {
            "Sid": "PublicReadGetObject",
            "Effect": "Allow",
            "Principal": "*",
            "Action": "s3:GetObject",
            "Resource": "YOUR-BUCKET-ARN/*"
        }
    ]
}
```
 - Test the access by locating a file in a bucket under Objects and "Copy S3 URI" for it.

#### (Optional) Connecting CloudWatch

For a production system, logging is essential, so you are informed about anomalies before customer complaints, or you are able to provide an evidence for an incident and so on.

Mind the region of your EC2 instance for these steps.

- [Create a CloudWatch log group](https://eu-west-1.console.aws.amazon.com/cloudwatch/home?region=eu-west-1#logsV2:log-groups/create-log-group), note down the ARN.
- Create a log stream inside the previously created log group, for instance `in`.
- Create an IAM user with an access key and secret with the following policy:
```json
{
	"Version": "2012-10-17",
	"Statement": [
		{
			"Effect": "Allow",
			"Action": [
				"logs:CreateLogStream",
				"logs:PutLogEvents",
				"logs:DescribeLogStreams"
			],
			"Resource": [
				"[YOUR-GROUP-ARN]",
				"[YOUR-GROUP-ARN]:*"
			]
		}
	]
}
```
- Configure the `services.vector` part in your `flake.nix` to activate logging:
```
services.vector = {
    enable = true;
    journaldAccess = true;
    settings = {
        sources.journald = {
            type = "journald";
            include_units = ["app.service" "nginx.service" "worker.service"];
        };
        transforms.remap_remove_specific_keys = {
             type = "remap";
             inputs = ["journald"];
             source = ''
                 del(._STREAM_ID)
                 del(._SYSTEMD_UNIT)
                 del(._BOOT_ID)
                 del(.source_type)
             '';
        };
        sinks.out = {
            auth = {
                access_key_id = "YOUR-IAM-ACCESS-KEY";
                secret_access_key = "YOUR-IAM-ACCESS-KEY";
            };
            inputs  = ["remap_remove_specific_keys"];
            type = "aws_cloudwatch_logs";
            compression = "gzip";
            encoding.codec = "json";
            region = "us-east-1";
            group_name = "tpp-qa";
            stream_name = "in";
        };
    };
};
```
- Review the incoming log entries, adjust remapping accordingly. You might want to remove or transform more entries to make the logs useful for alerts or accountability.

### Connecting to the EC2 / Virtual Machine Instance

After you've created the instance, configure your local SSH settings to point to the instance.

In your `~/.ssh/config` you typically add something like this:

```
Host ihp-app
    HostName ec2-.....compute.amazonaws.com
    User root
        IdentityFile /Users/marc/.ssh/ihp-app.pem
```

Now you can connect to the instance using `ssh ihp-app`.

### Configuring the Instance

In your `flake.nix` add the following configuration:

```nix
flake.nixosConfigurations."ihp-app" = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = inputs;
    modules = [
        "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
        ihp.nixosModules.appWithPostgres
        ({ ... }: {
            security.acme.defaults.email = "me@example.com";

            services.ihp = {
                domain = "myihpapp.com";
                migrations = ./Application/Migration;
                schema = ./Application/Schema.sql;
                fixtures = ./Application/Fixtures.sql;
                sessionSecret = "xxx";
            };

            # Add swap to avoid running out of memory during builds
            # Useful if your server have less than 4GB memory
            swapDevices = [ { device = "/swapfile"; size = 8192; } ];

            # This should reflect the nixos version from the NixOS AMI initally installed
            # After the initial install, it should not be changed. Otherwise e.g. the postgres
            # server might need a manual data migration if NixOS changes the default postgres version
            system.stateVersion = "23.05";
        })
    ];
};
```

In the first line the `"ihp-app"` needs to be the same as your SSH name from the previous section.

Make sure you put this into the `flake-parts.lib.mkFlake` block. The final `flake.nix` should look like this:

```diff
{
    inputs = {
        ihp.url = "github:digitallyinduced/ihp/v1.2";
        nixpkgs.follows = "ihp/nixpkgs";
        flake-parts.follows = "ihp/flake-parts";
        devenv.follows = "ihp/devenv";
        systems.follows = "ihp/systems";
    };

    outputs = inputs@{ ihp, flake-parts, systems, ... }:
        flake-parts.lib.mkFlake { inherit inputs; } {

            systems = import systems;
            imports = [ ihp.flakeModules.default ];

            perSystem = { pkgs, ... }: {
                ihp = {
                    enable = true;
                    projectPath = ./.;
                    packages = with pkgs; [
                        # Native dependencies, e.g. imagemagick
                    ];
                    haskellPackages = p: with p; [
                        # Haskell dependencies go here
                        p.ihp
                        cabal-install
                        base
                        wai
                        text
                    ];
                };
            };

+            flake.nixosConfigurations."ihp-app" = nixpkgs.lib.nixosSystem {
+                system = "x86_64-linux";
+                specialArgs = inputs;
+                modules = [
+                    "${nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
+                    ihp.nixosModules.appWithPostgres
+                    ({ ... }: {
+                        security.acme.defaults.email = "me@example.com";
+
+                        services.ihp = {
+                            domain = "myihpapp.com";
+                            migrations = ./Application/Migration;
+                            schema = ./Application/Schema.sql;
+                            fixtures = ./Application/Fixtures.sql;
+                            sessionSecret = "xxx";
+                        };
+
+                        # Job workers are active by default. Disable them like this:
+                        # systemd.services.worker.enable = pkgs.lib.mkForce false;
+
+                        # Add swap to avoid running out of memory during builds
+                        # Useful if your server have less than 4GB memory
+                        swapDevices = [ { device = "/swapfile"; size = 8192; } ];
+
+                        # This should reflect the nixos version from the NixOS AMI initally installed
+                        # After the initial install, it should not be changed. Otherwise e.g. the postgres
+                        # server might need a manual data migration if NixOS changes the default postgres version
+                        system.stateVersion = "23.05";
+                    })
+                ];
+            };

        };
}
```


### Deploying the App

Now you can deploy the app using `deploy-to-nixos` (which is just a small wrapper around nixos-rebuild):

```bash
deploy-to-nixos ihp-app
```

This will connect to the server via SSH and apply the NixOS configuration to the server.


### Backward-incompatible database update

If you have a backward-incompatible modification in the schema, you need to
recreate the database entirely, or you need an upgrade path.

Steps to do to start from scratch:
 - `make db` locally to have a clean local state.
 - `make sql_dump > /tmp/[your-app].sql`
 - `scp /tmp/[your-app].sql [your-app]-[env]:~`
 - `ssh [your-app]-[env]`
   - `systemctl stop app.service && (echo "drop database app with (force); create database app;" | psql -U postgres -h [your-db-server-host] -p [your-db-server-port] postgres)`
   - `cat [your-app] | psql -U postgres -h [your-db-server-host] -p [your-db-server-port] app` # Consult flake.nix for the values in case.
   - `rm [your-app].sql`
   - `systemctl start app.service`
   - `exit`
 - `rm /tmp/[your-app].sql`


### Troubleshooting / operations

If a deployment or the initial creation goes wrong, there are techniques to locate the root cause, first, login to the EC2 instance:
`ssh [your-app]-[env]`
If logging in does not work, let's open AWS dashboard and initiate a reboot.
After you logged in, you can:
 - Check resource usages `df -h`, `free -m`, `top` to see if the instance capacity is okay for the deployment / load.
 - `systemctl start app.service` / `systemctl restart app.service` check if the app can be started manually
 - Check app logs: `journalctl --unit=app.service -n 100 --no-pager`
 - Check worker logs: `journalctl  -u worker -r`
 - Delete old logs if disk is full: `journalctl --vacuum-time=2d` - keep only past 2 days for example
 - `dmesg` to spot any hardware/virtualization anomalies.
 - `iptables -L` to see firewall rules, in case of network connectivity issues.

Keep in mind that changes should be always done declaratively, via the `nix` files, for example changing the firewall rules temporarily via `iptables` will be lost at the next deployment, so `ssh` into the instance is merely for debugging, locating the root cause. The solution almost always involves a change in the `flake.nix` for the sake of idempotence.

## Deploying with Docker

Deploying IHP with docker is a good choice for a professional production setup.

IHP has a first party CLI tool called `ihp-app-to-docker-image` to create Docker images out of your app. This tool is available [with IHP Pro and IHP Business](ihp-pro.html). If you're not on IHP Pro yet, now is a good time to try it out. By switching to Pro, you're supporting the sustainable development of IHP.

### Creating a Docker Image

To create a Docker image, we first need to install [Podman](https://podman.io/), and then run the following command:

```bash
nix build .#unoptimized-docker-image --option sandbox false --extra-experimental-features nix-command --extra-experimental-features flakes

cat result | podman load
```

Running `podman images` you can now see that the image is available:

```bash
$ docker images

REPOSITORY     TAG                                IMAGE ID       CREATED         SIZE
app            g13rks9fb4ik8hnqip2s3ngqq4nq14zw   ffc01de1ec7e   54 years ago    86.6MB
```

The `CREATED` timestamp is showing over 50 years ago as the image is built using nix. For having a totally reproducible build, the timestamp is set to `Jan 1970, 00:00 UTC`.

### Starting the App Container

#### First Steps

You can start your app container like this:

```bash
$ docker run -p 8000:8000 app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

Now open `http://localhost:8000/` and see that your app is running. It will likely show an error that IHP is unable to connect to the app DB. This will be fixed in the next section.

The app image has to be the fully qualified `app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw` form, so this will not work:

```bash
$ docker run -p 8000:8000 app

Unable to find image 'app:latest' locally
```

#### Connecting the DB

You need to connect a postgres database to get your app working.

It's recommended to use a managed database service like AWS RDS to run your postgres. For a quick-and-dirty setup you can also use docker to run a database:

```bash
$ docker run --name app-db -e POSTGRES_PASSWORD=mysecretpassword -d postgres

# Import the Schema.sql
$ docker exec -i app-db psql -U postgres -d postgres < Application/Schema.sql
CREATE EXTENSION
CREATE TABLE
...

# Import the Fixtures.sql
$ docker exec -i app-db psql -U postgres -d postgres < Application/Fixtures.sql
ALTER TABLE
INSERT 0 1
INSERT 0 1
INSERT 0 1
...

```

You can configure the database your app connects to using the `DATABASE_URL` env variable:

```bash
$ docker run \
    -p 8000:8000 \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

#### Recommended Env Variables

##### `IHP_SESSION_SECRET`

In production setup's you want to configure the `IHP_SESSION_SECRET` env variable. It's a private key used to encrypt your session state. If it's not specified, a new one will generated on each container start. This means that all your users will have to re-login on each container start.

**Note on `Config/client_session_key.aes`:** The `IHP_SESSION_SECRET` env variable is an alternative for placing a `Config/client_session_key.aes` inside the container. It has been added in recent IHP versions only.

When you start an app container without specifying the `IHP_SESSION_SECRET`, the app will output the randomly generated one. So you can get a new secret key by starting a new container and copying the value:

```bash
$ docker run -it app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp
Server started
```

There we can copy the `IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp` value and use it as our secret:

```bash
$ docker run \
    -p 8000:8000 \
    -e 'IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp' \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_ASSET_VERSION`

**As of IHP v0.16 this env variable is automatically set to a unqiue build hash.**

If you use [`assetPath` helpers](assets.html) in your app, specifiy the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

```bash
$ docker run \
    -p 8000:8000 \
    -e 'IHP_SESSION_SECRET=1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp' \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    -e 'IHP_ASSET_VERSION=af5f389ef7a64a04c9fa275111e4739c0d4a78d0' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

```bash
$ docker run \
    -e 'IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

##### `IHP_BASEURL`

Without specifying this env var, the app will always use `http://localhost:8000/` in absolute URLs it's generating (e.g. when redirecting or sending out emails).

It's therefore important to set it to the external user-facing web addresss. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

```bash
$ docker run \
    -e 'IHP_BASEURL=https://example.com' \
    app:g13rks9fb4ik8hnqip2s3ngqq4nq14zw
```

### TLS certificates in Nix-built Docker images

If your container makes HTTPS requests (e.g. Google OAuth, GitHub API, S3) and you see errors like:

```
HttpExceptionRequest ... (InternalException (HandshakeFailed (Error_Protocol "certificate has unknown CA" UnknownCa)))
```

your image likely does not contain a root CA bundle. Minimal images produced by `dockerTools.buildImage` do not include `/etc/ssl/certs` by default.

Fix by overriding the IHP Docker image to include CA certificates and set standard SSL env vars so libraries can find them:

```nix
# inside your flake outputs, override the image used by nix build .#unoptimized-docker-image
packages = {
  unoptimized-docker-image = lib.mkForce (pkgs.dockerTools.buildImage {
    name = "ihp-app";
    # Provide a minimal userspace and CA bundle
    copyToRoot = with pkgs.dockerTools; [
      usrBinEnv   # /usr/bin/env for scripts
      binSh       # /bin/sh for shell scripts
      caCertificates  # /etc/ssl/certs/ca-certificates.crt
      fakeNss     # NSS files for name resolution
    ];
    config = {
      Cmd = [ "${self'.packages.unoptimized-prod-server}/bin/RunProdServer" ];
      Env = [
        "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
        "NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt"
        "SSL_CERT_DIR=/etc/ssl/certs"
      ];
    };
  });
};
```

## Deploying on Bare Metal

You can build and deploy your IHP app on your own server without external deployment tools.

Make sure that the infrastructure you pick to build your IHP app has enough memory. Otherwise, the build might fail because GHC is very memory hungry. You can also set up a swap file to work around this.

### Install Nix on your server

Nix is needed to build your application. Install it the usual way:

```bash
curl -L https://nixos.org/nix/install | sh
```

We recommend to use the digitally induced cachix binary cache to avoid rebuilding the IHP dependencies and IHP itself:

```
cachix use digitallyinduced
```

In case you're on NixOS, you can skip this.

### Copy your project folder to your server

Copy your application source code to the build server. If you're using `git` to clone it onto your server, we recommend you use [`SSH agent forwarding`](https://docs.github.com/en/developers/overview/using-ssh-agent-forwarding).

### Configuration

IHP apps are typically configured using environment variables:

1. Set the env `IHP_ENV=Production` to enable production mode
2. Set `IHP_BASEURL=https://{yourdomain}`
3. Configure any custom settings
   (This includes ´devenv up´ to download and build any extra Haskell packages, such as the mmark package in the tutorial)

If you deploy behind an Nginx proxy or similar which handles SSL certificates, so the IHP instance only sees http, the `IHP_BASEURL` must still have `https` as it is used to form absolute URLs.

To configure your database connection: Set the env var `DATABASE_URL` to your Postgres connection URL.
Set the env var `PORT` to the port the app will listen on.

The database needs the UUID-extension which is enabled by running `create extension if not exists "uuid-ossp";`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

#### Tweaking memory usage and performance

IHP by default sets some default values for the GHC/Haskell Runtime System (RTS) which work well in production with high loads, at the cost of using a bit of memory. If you want your IHP deployment to use less RAM on your machine, try different values for the environment variable `GHCRTS`. The default value (set in IHP's [Makefile.dist](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/Makefile.dist#L86)) is `"-A128M -N3 -qn3 -G4"`. If you want very low memory usage, at the cost of more frequent garbage collection, try `export GHCRTS="-A16M -N"`. A middle ground is IHPCloud's default of `export GHCRTS="-A128M -N3 -qn3 -G4"`.

For explanations of these values, see GHC's [manual on the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html) and on [RTS options for concurrency]( https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism). In short, `-A` is the allocation area, `-G` is number of generations, `-qn` is number of threads to use for parallel GC, `-N` is number of threads and `-n` is the memory chunk area.

### Building

First run `make prepare-optimized-nix-build` to enable optimized binary builds. You can skip this step in case you want faster build times, and are fine with slower runtime performance.

Inside your project directory call `nix-build`. This will trigger a full clean build and place the output at `./result`.

After the build has finished, you can find the production binary at `result/bin/RunProdServer`.

### Starting the app

Now you should be able to start your app by running `result/bin/RunProdServer`.

## CSS & JS Bundling

Bundling all your CSS and JS files into a single CSS and JS file can be useful to improve performance when you have many assets.

### Caching

To decide whether bundling is useful for your application it might be useful to quickly go into details about the browser caching that is applied to all static files.

In production mode IHP automatically adds caching headers to your CSS and JS files following these rules:

1. `static/vendor/*`: cached for 30 days
2. `static/*` cached for 24 hours
3. IHP built-ins (e.g. `helpers.js`, `ihp-auto-refresh.js`, ..): cached for 30 days

This means that all JS and CSS is stored in the browser cache after the first request. This also means that bundling only improves the cache-miss case. To optimise the first request it could be useful to move more scripts from the `<head>` to the end of the `<body>` in your `Layout.hs`. Usually not all CSS and JS is needed when the page is first displayed.

If you have many JS and CSS files that are all required for the initial page render, you should enable bundling.

If you're curious: The following cache headers are set in production:

-   `Cache-Control`
-   `Last-Mofified`
-   `ETag`

### Activate Bundling

IHP provides a simple bundling out of the box using `make`. These bundles can be generated using these commands:

```bash
make static/prod.js # Generates a bundle at static/prod.js
make static/prod.css # Generates a bundle at static/prod.css
```

The bundling process is only concatenating the files (along the lines of `cat a.css b.css c.css > static/prod.css`). There is no minification or transpiling applied.

Bundling should only be used in production. In development mode you should generally not use the bundling mechanism.

#### Configuring the CSS & JS Bundling

The files that are bundled together to get our `prod.css` and `prod.js` are configured in your projects `Makefile`. Inside your projects `Makefile` you will have these statements:

```bash
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css

JS_FILES += ${IHP}/static/vendor/jquery-3.6.0.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper-2.11.6.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/morphdom-umd.min.js
JS_FILES += ${IHP}/static/vendor/turbolinks.js
JS_FILES += ${IHP}/static/vendor/turbolinksInstantClick.js
JS_FILES += ${IHP}/static/vendor/turbolinksMorphdom.js
```

You need to add your app-specific CSS and JS files here as well. E.g. if you have an `app.css`, `layout.css` and `app.js` add them by appending this:

```bash
CSS_FILES += static/app.css
CSS_FILES += static/layout.css
JS_FILES += static/app.js
```

Run `make static/prod.js static/prod.css` to test that the bundle generation works locally. To force a rebuild, either delete the files and run make again, or run `make -B static/prod.js static/prod.css`.

You can also remove the JS and CSS files that are provided by IHP (like `${IHP}/static/vendor/bootstrap.min.css`) if you don't need them. E.g. if you don't use bootstrap for your CSS, just remove the `CSS_FILES` and `JS_FILES` statements for bootstrap.

**Note on CSS Imports:**

If your app.css uses `@import` syntax like this:

```css
@import './layout.css';
@import './startpage.css';
```

Browsers only load these `@import` statements if they're the first rules defined in your CSS file. When bundling your file, you usually have your CSS frameworks and libraries first before your own app specific CSS. That means that the `@import` statements will be ignored by the browser in production. To make this work in production you need to duplicate these import statements in your `Makefile` like this:

```makefile
# CSS Frameworks
CSS_FILES += ${IHP}/static/vendor/bootstrap.min.css
CSS_FILES += ${IHP}/static/vendor/flatpickr.min.css

CSS_FILES += static/app.css # The main app.css
CSS_FILES += static/layout.css # The first import of app.css
CSS_FILES += static/startpage.css # The second import of app.css
```

### Enabling Bundling in the Layout

We need to update the `Layout.hs` to only load `prod.css` and `prod.js` when running in production.

For that we use [`isDevelopment`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#v:isDevelopment) and [`isProduction`](https://ihp.digitallyinduced.com/api-docs/IHP-FrameworkConfig.html#v:isProduction) to conditionally load different files. Change your `Web/View/Layout.hs` to look like this:

```haskell
stylesheets :: Html
stylesheets = do
    when isDevelopment [hsx|
        <link rel="stylesheet" href="/vendor/bootstrap.min.css"/>
        <link rel="stylesheet" href="/vendor/flatpickr.min.css"/>
        <link rel="stylesheet" href="/app.css"/>
    |]
    when isProduction [hsx|
        <link rel="stylesheet" href="/prod.css"/>
    |]

scripts :: Html
scripts = do
    when isDevelopment [hsx|
        <script id="livereload-script" src="/livereload.js"></script>
        <script src="/vendor/jquery-3.6.0.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper-2.11.6.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
        <script src="/vendor/morphdom-umd.min.js"></script>
    |]
    when isProduction [hsx|
        <script src="/prod.js"></script>
    |]
```

### Updating your Deployment Process

**If you're using Nix:** Nothing to do. The command `make static/prod.js static/prod.css` is automatically executed during deployment.

**If you're deploying manually:** Make sure that `make static/prod.js static/prod.css` is called.

## Operating an IHP app

### Error Monitoring with Sentry

In production it's highly recommended to use an exception tracking service like [Sentry](https://sentry.io/) for error monitoring.

To use sentry in your IHP app you need to install the ihp-sentry plugin. The ihp-sentry plugin is bundled with IHP Pro.

Once the `ihp-sentry` plugin is installed and configured, exceptions that happen in production (so `option Production` is set) are reported to sentry.

#### Install ihp-sentry in your IHP app

Add `ihp-sentry` to the `haskellDeps` in your `default.nix`:

```nix
let
    ...
    haskellEnv = import "${ihp}/NixSupport/default.nix" {
        ihp = ihp;
        haskellDeps = p: with p; [
            # ...
            ihp-sentry
        ];
    ...
```

Now you need to remake your environment using `devenv up`.

Next add `import IHP.Sentry` to your `Config/Config.hs`:

```haskell
module Config where

-- ...

import IHP.Sentry
```

Add a call to `initSentry` inside the `Config/Config.hs` to configure the sentry DSN:

```haskell
module Config where

import IHP.Prelude
import IHP.Environment
import IHP.FrameworkConfig
import IHP.Sentry

config :: ConfigBuilder
config = do
    option Development
    option (AppHostname "localhost")

    initSentry "YOUR-SENTRY-DSN"
```

Now sentry is set up.

## Building with Nix

You can use `nix-build` to make a full build of your IHP app:

```
# Optional, if you skip this the binary will not be optimized by GHC
make prepare-optimized-nix-build

# The actual build process
nix-build
```

This will build a nix package that contains the following binaries:

-   `RunProdServer`, the binary to start web server
-   `RunJobs`, if you're using the IHP job queue, this binary will be the entrypoint for the workers
-   a binary for each script in `Application/Script`, e.g. `Welcome` for `Application/Script/Welcome.hs`

The build contains an automatic hash for the `IHP_ASSET_VERSION` env variable, so cache busting should work out of the box.


# Env Var Reference

Your IHP app can be configured at runtime with environment variables.

## Recommended Env Vars


#### `IHP_ENV`

Switch IHP to production mode like this:

```bash
$ export IHP_ENV="Production"
$ ./build/bin/RunProdServer
```

The production mode has effects on caching behaviour of static files, logging and error rendering.

You can also use `IHP_ENV=Development` to force dev mode.


#### `IHP_BASEURL`

Without specifying this env var, the app will always use `http://localhost:8000/` in absolute URLs it's generating (e.g. when redirecting or sending out emails).

It's therefore important to set it to the external user-facing web addresss. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

```bash
$ export IHP_BASEURL=https://example.com
$ ./build/bin/RunProdServer
```

#### `IHP_SESSION_SECRET`

In production setup's you want to configure the `IHP_SESSION_SECRET` env variable. It's a private key used to encrypt your session state. If it's not specified, a new one will generated on each app start. This means that all your users will have to re-login on each app start.

**Note on `Config/client_session_key.aes`:** The `IHP_SESSION_SECRET` env variable is an alternative for placing a `Config/client_session_key.aes` inside the your repository. If IHP detects a `Config/` folder, and no `IHP_SESSION_SECRET` is set, it will automatically create a `Config/client_session_key.aes` file. This is designed for persistent sessions in development mode.

When you start an app without specifying the `IHP_SESSION_SECRET` and no `Config/client_session_key.aes` is found, the app will output the randomly generated one. So you can get a new secret key by starting a new container and copying the value.

An easier way is to use the `new-session-secret` CLI command:

```bash
$ new-session-secret
1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp
```

On macOS you can directly copy this into your clipboard like this:

```bash
$ new-session-secret | pbcopy
```

Then you can paste the value where needed.

Now we can use this secret and pass it to the app binary via the `IHP_SESSION_SECRET` env var:

```bash
$ export IHP_SESSION_SECRET="1J8jtRW331a0IbHBCHmsFNoesQUNFnuHqY8cB5927KsoV5sYmiq3DMmvsYk5S7EDma9YhqZLZWeTFu2pGOxMT2F/5PnifW/5ffwJjZvZcJh9MKPh3Ez9fmPEyxZBDxVp"
$ ./build/bin/RunProdServer
```

#### `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE`

If the app is running behind a load balancer, set the environment variable `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader` to tell IHP to use the `X-Real-IP` or `X-Forwarded-For` header for detecting the client IP.

```bash
$ export IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader
$ ./build/bin/RunProdServer
```

#### `PORT`

Specifies the TCP Port where the application will listen to. Defaults to `8000`.

```bash
$ export PORT=1337
$ ./build/bin/RunProdServer
# App now starts on port 1337 instead of 8000
```

#### `DATABASE_URL`

You can configure the database your app connects to using the `DATABASE_URL` env variable:

```bash
$ export DATABASE_URL="postgresql://postgres:mysecretpassword@the-hostname/postgres"
$ ./build/bin/RunProdServer
```


## Advanced Env Vars

#### `IHP_ASSET_VERSION`

As of IHP v0.16 this env variable is automatically set to a unique build hash.

If you use [`assetPath` helpers](assets.html) in your app, specifiy the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

```bash
$ export IHP_ASSET_VERSION=af5f389ef7a64a04c9fa275111e4739c0d4a78d0
$ ./build/bin/RunProdServer
```

#### `GHCRTS`

IHP by default sets some default values for the GHC/Haskell Runtime System (RTS) which work well in production with high loads, at the cost of using a bit of memory. If you want your IHP deployment to use less RAM on your machine, try different values for the environment variable `GHCRTS`. The default value (set in IHP's [Makefile.dist](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/Makefile.dist#L86)) is `"-A128M -N3 -qn3 -G4"`. If you want very low memory usage, at the cost of more frequent garbage collection, try `export GHCRTS="-A16M -N"`. A middle ground is IHPCloud's default of `export GHCRTS="-A128M -N3 -qn3 -G4"`.

For explanations of these values, see GHC's [manual on the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html) and on [RTS options for concurrency]( https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism). In short, `-A` is the allocation area, `-G` is number of generations, `-qn` is number of threads to use for parallel GC, `-N` is number of threads and `-n` is the memory chunk area.

#### `IHP_IDE_BASEURL`

Only relevant in dev mode. This defaults to `IHP_IDE_BASEURL=http://localhost:8001`.

#### `IHP_RLS_AUTHENTICATED_ROLE`

A database role used by IHP DataSync. Defaults to `ihp_authenticated`.

#### `IHP_DATASYNC_MAX_SUBSCRIPTIONS_PER_CONNECTION`

The maximum number of subscriptions per websocket connection in IHP DataSync. Defaults to `128`.

#### `IHP_DATASYNC_MAX_TRANSACTIONS_PER_CONNECTION`

The maximum number of database transactions per websocket connection in IHP DataSync. Defaults to `10`.

## `systemd` Integration

The `deploy-to-nixos` tool now includes systemd integration to improve reliability and reduce downtime for IHP applications. These features are enabled by default when using `ihp.nixosModules.app`.

Key Features:

1. **Systemd Watchdog**:

   - The app sends a heartbeat to systemd every 30 seconds via localhost.
   - If the app becomes unresponsive, systemd restarts it after 60 seconds.

2. **Socket Activation**:

   - Systemd queues incoming HTTP requests during app startup or restarts.
   - This eliminates downtime and ensures uninterrupted service.

3. **Automatic Configuration**:

   - The `IHP_SYSTEMD` environment variable is set to `"1"` automatically when deploying with `deploy-to-nixos`. If you are deploying differently, you are responsible for setting the variable yourself.


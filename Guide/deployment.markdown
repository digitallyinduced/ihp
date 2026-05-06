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

Now the NixOS instance and Postgres database is setup and an SSH connection can be established to it.

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
 - Edit `Config/nix/hosts/production/configuration.nix` and uncomment the `databaseUrl` line under `services.ihp`, setting it to: `databaseUrl = lib.mkForce "postgresql://postgres:YOUR-PASSWORD@YOUR-HOSTNAME.amazonaws.com/postgres";`. You can find the proper hostname after the initialization is complete, on the RDS instance detail page.
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
- Configure the `services.vector` part in your `Config/nix/hosts/production/configuration.nix` to activate logging (uncomment the commented-out `services.vector` block and fill in your values):
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
Host production
    HostName ec2-.....compute.amazonaws.com
    User root
    IdentityFile ~/.ssh/ihp-app.pem
```

The SSH host name must match the NixOS configuration name used in your `flake.nix`. Projects generated by `ihp-new` use `production` as the default configuration name, so we use `production` as the SSH host name here.

Now you can connect to the instance using `ssh production`.

### Configuring the Instance

Projects generated by `ihp-new` already include deployment configuration files under `Config/nix/hosts/production/`. The file structure is:

```
Config/nix/hosts/production/
├── host.nix                  # Entry point, sets system architecture and imports configuration.nix
├── configuration.nix         # Main config: domain, ACME, nginx, services.ihp, firewall, etc.
└── hardware-configuration.nix # Hardware-specific config (e.g. AWS EC2 AMI import)
```

Your `flake.nix` already references this configuration:

```nix
flake.nixosConfigurations."production" = import ./Config/nix/hosts/production/host.nix { inherit inputs; };
```

#### Editing `configuration.nix`

Open `Config/nix/hosts/production/configuration.nix` and update the following placeholder values:

1. **ACME email** (for Let's Encrypt certificates):
   ```nix
   security.acme.defaults.email = "you@example.com";
   ```

2. **Domain name** — update in both the nginx virtual host and the IHP service:
   ```nix
   services.nginx = {
       virtualHosts."yourdomain.com" = {};
   };

   services.ihp = {
       domain = "yourdomain.com";
       # ...
   };
   ```

3. **Session secret** — generate one with `new-session-secret` and set it:
   ```nix
   services.ihp = {
       sessionSecret = "your-generated-secret";
       # ...
   };
   ```

4. **SMTP and AWS credentials** — update the `CHANGE-ME` values in `additionalEnvVars` if you use email or S3 file storage.

5. **(Optional) CloudWatch logging** — uncomment and configure the `services.vector` block if you want log forwarding.

#### Editing `hardware-configuration.nix`

For AWS EC2 deployments, uncomment the Amazon AMI import in `Config/nix/hosts/production/hardware-configuration.nix`:

```nix
imports = [
    "${inputs.nixpkgs}/nixos/modules/virtualisation/amazon-image.nix"
];
```

For non-AWS deployments, run `nixos-generate-config` on your server and copy the generated hardware configuration into this file.

#### Choosing the Right NixOS Module

IHP provides two main NixOS modules:

- **`ihp.nixosModules.appWithPostgres`** — All-in-one: sets up your app, worker, migrations, nginx, PostgreSQL, Let's Encrypt, firewall, swap, and nix GC. Use this when deploying to a single server with a local database.
- **`ihp.nixosModules.app`** — Just the app services (app, worker, migrations, keygen) without PostgreSQL or nginx. Use this when you manage PostgreSQL separately (e.g. AWS RDS) or configure nginx yourself.

You can also import individual service modules (`ihp.nixosModules.services_app`, `services_worker`, `services_migrate`, `services_appKeygen`) for full control.

#### Separate Nixpkgs for NixOS Deployments

IHP provides two `nixpkgs` inputs that can be pinned independently:

- **`nixpkgs`** — Used for building Haskell packages and development. This can track the latest nixpkgs for newer GHC versions and updated Haskell libraries.
- **`nixpkgs-nixos`** — Used for NixOS server deployments (system packages like nginx, PostgreSQL, etc.). This can be pinned to a stable NixOS release for production reliability.

By default both point to the same nixpkgs version. To pin them independently, override the URLs in your app's `flake.nix`:

```nix
inputs = {
    ihp.url = "github:digitallyinduced/ihp/v1.4";
    nixpkgs.follows = "ihp/nixpkgs";
    nixpkgs-nixos.follows = "ihp/nixpkgs-nixos";
    # ... other inputs
};
```

Then in your `host.nix`, use `nixpkgs-nixos` for the NixOS system evaluation:

```nix
{ inputs }:
inputs.nixpkgs-nixos.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = inputs // { nixpkgs = inputs.nixpkgs-nixos; };
    modules = [ ./configuration.nix ];
}
```

This ensures NixOS system packages (nginx, PostgreSQL, vim, etc.) come from the stable `nixpkgs-nixos` input, while your IHP application binary remains built against the Haskell `nixpkgs` — it is a pre-built derivation from `self.packages.*` and is unaffected by the NixOS nixpkgs choice.

#### Security Hardening

Add these settings to your `configuration.nix` for a hardened production server:

```nix
# Firewall — allow HTTP, HTTPS, and SSH
networking.firewall.enable = true;
networking.firewall.allowedTCPPorts = [ 80 443 22 ];

# SSH — key-only authentication
services.openssh.enable = true;
services.openssh.settings.PasswordAuthentication = false;

# Brute-force protection
services.fail2ban.enable = true;
```

You can also harden the app and worker systemd services:

```nix
# Private /tmp for the app (prevents temp file attacks)
systemd.services.app.serviceConfig.PrivateTmp = true;
systemd.services.worker.serviceConfig.PrivateTmp = true;

# If your app writes files (uploads, exports), use StateDirectory
# Files will be stored under /var/lib/mydata/
systemd.services.app.serviceConfig.StateDirectory = "mydata";
systemd.services.worker.serviceConfig.StateDirectory = "mydata";

# DynamicUser runs the service as an ephemeral system user
# Combined with StateDirectory, the app can only write to its own directory
systemd.services.app.serviceConfig.DynamicUser = true;
```

When using `StateDirectory` with IHP's file storage, set `IHP_STORAGE_DIR` to point at the directory using the systemd `%S` specifier (which expands to `/var/lib`):

```nix
systemd.services.app.environment.IHP_STORAGE_DIR = "%S/mydata/";
```

#### Automatic Upgrades and Kernel Reboots

`system.autoUpgrade.enable = true;` rebuilds the system on a schedule and installs new kernels into `/boot`, but without `allowReboot` the host keeps running the old kernel until rebooted manually. To pick up kernel patches automatically, enable reboots and pin them to a quiet window:

```nix
system.autoUpgrade = {
    enable = true;
    allowReboot = true;
    dates = "Sun 04:00";
    randomizedDelaySec = "30min";
};
```

Reboots only happen when a new kernel or initrd was actually installed. `services.app` and `services.worker` come back automatically thanks to `Restart = "always"`; for any custom units, ensure `wantedBy = [ "multi-user.target" ];` is set. Verify with:

```bash
systemctl list-timers nixos-upgrade.timer
```

#### Nginx Configuration Patterns

The `appWithPostgres` module sets up a basic nginx proxy with WebSocket support. Here are additional patterns you can add to your `configuration.nix`:

**Domain redirects:**

```nix
# Redirect www to apex domain (or vice versa)
services.nginx.virtualHosts."www.example.com" = {
    enableACME = true;
    forceSSL = true;
    globalRedirect = "example.com";
};
```

**502 retry during deploys** — when the app restarts, nginx briefly gets 502 errors. This retry pattern makes deploys transparent to users:

```nix
services.nginx.virtualHosts."example.com" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
        proxyPass = "http://localhost:8000";
        proxyWebsockets = true;
        extraConfig = ''
            recursive_error_pages on;
            proxy_intercept_errors on;
            error_page 502 = @retry;
        '';
    };
    locations."@retry" = {
        proxyPass = "http://localhost:8000";
        extraConfig = ''
            proxy_connect_timeout 2s;
        '';
    };
};
```

**File upload size limit:**

```nix
services.nginx.clientMaxBodySize = "100m";
```

**High-performance tuning** — for servers handling many concurrent connections, increase worker limits and enable modern compression:

```nix
services.nginx = {
    recommendedZstdSettings = true;
    recommendedBrotliSettings = true;
    eventsConfig = ''
        worker_connections 81920;
        multi_accept on;
    '';
    appendConfig = ''
        worker_processes auto;
        worker_rlimit_nofile 262144;
    '';
};

# Also raise systemd limits for the nginx process
systemd.services.nginx.serviceConfig.LimitNOFILE = 262144;
systemd.services.nginx.serviceConfig.LimitNPROC = 65536;
```

**IP blocking** — block abusive IPs at the nginx level:

```nix
services.nginx.commonHttpConfig = ''
    deny 1.2.3.4;
    deny 5.6.7.8;
'';
```

**Wildcard SSL certificates** (for SaaS apps with customer subdomains):

```nix
# Wildcard cert via DNS validation (example using INWX provider)
security.acme.certs."example.com" = {
    domain = "*.example.com";
    dnsProvider = "inwx";  # or cloudflare, route53, etc.
    group = config.services.nginx.group;
    credentialFiles = {
        INWX_USERNAME_FILE = "/root/inwx.user";
        INWX_PASSWORD_FILE = "/root/inwx.pass";
    };
};

services.nginx.virtualHosts."*.example.com" = {
    useACMEHost = "example.com";
    forceSSL = true;
    locations."/" = {
        proxyPass = "http://localhost:8000";
        proxyWebsockets = true;
    };
};
```

See the [ACME NixOS documentation](https://nixos.org/manual/nixos/stable/#module-security-acme) for supported DNS providers.

#### PostgreSQL Production Tuning

The `appWithPostgres` module sets up PostgreSQL with sensible defaults. For production workloads, you may want to tune these settings in your `configuration.nix`:

```nix
# Increase connection limit (default is 100)
services.postgresql.settings.max_connections = 1000;

# Add extensions
services.postgresql.extensions = plugins: [ plugins.pg_uuidv7 ];

# Automated daily backups
services.postgresqlBackup.enable = true;
```

For tighter security on the database, configure authentication rules:

```nix
services.postgresql.authentication = ''
    local all all trust
    host all all 127.0.0.1/32 trust
    host all all ::1/128 trust
    host all all 0.0.0.0/0 reject
'';
```

This trusts local connections (from the app on the same server) and rejects all remote connections.

If you're hosting multiple apps (see [Multi-App Hosting](#hosting-multiple-ihp-apps-on-one-server)), use `ensureDatabases` and `ensureUsers` for declarative database provisioning instead of creating databases manually:

```nix
services.postgresql = {
    ensureDatabases = [ "myapp" "secondapp" ];
    ensureUsers = [
        { name = "myapp"; ensureDBOwnership = true; }
        { name = "secondapp"; ensureDBOwnership = true; }
    ];
};
```

NixOS will create these databases and users automatically on first boot. Each user gets ownership of its matching database via `ensureDBOwnership`.

### Deploying the App

Now you can deploy the app using `deploy-to-nixos` (which is just a small wrapper around nixos-rebuild):

```bash
deploy-to-nixos production
```

This will connect to the server via SSH and apply the NixOS configuration to the server. The argument must match both your SSH host name and the `nixosConfigurations` key in your `flake.nix`.


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

Keep in mind that changes should be always done declaratively, via the `nix` files, for example changing the firewall rules temporarily via `iptables` will be lost at the next deployment, so `ssh` into the instance is merely for debugging, locating the root cause. The solution almost always involves a change in the NixOS configuration files (under `Config/nix/hosts/production/`) for the sake of idempotence.

## Hosting Multiple IHP Apps on One Server

You can deploy multiple IHP applications on a single NixOS server. The primary app uses `ihp.nixosModules.appWithPostgres` as usual, and additional apps are added as flake inputs with their own systemd services, sockets, and nginx virtual hosts.

### Adding a Second App as a Flake Input

In your main project's `flake.nix`, add the second app as an input:

```nix
{
    inputs = {
        ihp.url = "...";
        # Add additional IHP apps:
        secondapp.url = "git+ssh://git@github.com/your-org/second-app";
    };
    # ...
}
```

### Systemd Services for the Secondary App

Create a new nix file (e.g. `Config/nix/hosts/production/secondapp.nix`) and import it from your `configuration.nix`:

```nix
{ config, pkgs, lib, self, ... }:
let
    ihpEnv = {
        PORT = "8090";
        IHP_REQUEST_LOGGER_IP_ADDR_SOURCE = "FromHeader";
        IHP_BASEURL = "https://secondapp.example.com";
        IHP_ENV = "Production";
        DATABASE_URL = "host=127.0.0.1 port=5432 dbname=secondapp user=secondapp";
        IHP_SESSION_SECRET = "generate-a-unique-secret-for-this-app";
        GHCRTS = "-A64m -N4 -H32m";
        IHP_SYSTEMD = "1";
    };
    package = self.inputs.secondapp.packages."${pkgs.system}".optimized-prod-server;
in
{
    # Web server
    systemd.services.secondapp = {
        enable = true;
        description = "secondapp web server";
        after = [ "network.target" "secondapp.socket" ];
        serviceConfig = {
            Type = "notify";
            ExecStart = "${package}/bin/RunProdServer";
            Restart = "always";
            KillSignal = "SIGINT";
            WatchdogSec = "60";
            Sockets = "secondapp.socket";
            TimeoutStopSec = "30s";
        };
        environment = ihpEnv;
        wantedBy = [ "multi-user.target" ];
    };

    # Socket activation on port 8090
    systemd.sockets.secondapp = {
        wantedBy = [ "sockets.target" ];
        socketConfig = {
            ListenStream = "8090";
            Accept = "no";
        };
    };

    # Background worker (inherits env from app, overrides GHCRTS)
    systemd.services.secondapp-worker = {
        enable = true;
        description = "secondapp background worker";
        serviceConfig = {
            Type = "simple";
            ExecStart = "${package}/bin/RunJobs";
            Restart = "on-failure";
        };
        environment = ihpEnv // { GHCRTS = "-A1M -N1 -qn1"; };
        wantedBy = [ "multi-user.target" ];
    };

    # Database migrations
    systemd.services.secondapp-migrate =
        let
            filter = self.inputs.secondapp.inputs.ihp.inputs.nix-filter.lib;
        in {
        serviceConfig = {
            Type = "oneshot";
            ExecStart = self.inputs.secondapp.inputs.ihp.apps."${pkgs.system}".migrate.program;
        };
        environment = {
            DATABASE_URL = ihpEnv.DATABASE_URL;
            MINIMUM_REVISION = "0";
            IHP_MIGRATION_DIR =
                let migrationFiles = filter {
                    root = self.inputs.secondapp;
                    include = [ "Application/Migration" (filter.matchExt "sql") ];
                    exclude = [ "Application/Schema.sql" "Application/Fixtures.sql" ];
                    name = "secondapp-migrations";
                };
                in "${migrationFiles}/Application/Migration/";
        };
        wantedBy = [ "multi-user.target" ];
        after = [ "postgresql.service" ];
    };

    # Nginx virtual host
    services.nginx.virtualHosts."secondapp.example.com" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
            proxyPass = "http://127.0.0.1:8090";
            proxyWebsockets = true;
        };
    };
}
```

Import this file from your main `configuration.nix`:

```nix
imports = [ ./hardware-configuration.nix ./secondapp.nix ];
```

### Multiple Databases

Each app typically gets its own database. The preferred approach is declarative provisioning via `ensureDatabases` and `ensureUsers` in your `postgres.nix` or `configuration.nix`:

```nix
services.postgresql = {
    ensureDatabases = [ "myapp" "secondapp" ];
    ensureUsers = [
        { name = "myapp"; ensureDBOwnership = true; }
        { name = "secondapp"; ensureDBOwnership = true; }
    ];
};
```

NixOS creates these on first boot. For the initial schema, SSH in and run:

```bash
sudo -u postgres psql secondapp < /path/to/Application/Schema.sql
```

Alternatively, create databases manually:

```bash
ssh production
sudo -u postgres createuser secondapp
sudo -u postgres createdb -O secondapp secondapp
```

### Multi-Tenant Pattern

If you run the same app against multiple databases (e.g. one per customer tenant), use `let` functions to avoid duplicating service definitions:

```nix
let
    migrate-service = databaseUrl: minimumRevision: {
        serviceConfig = {
            Type = "oneshot";
            ExecStart = self.inputs.myapp.inputs.ihp.apps."${pkgs.system}".migrate.program;
        };
        environment = {
            DATABASE_URL = databaseUrl;
            MINIMUM_REVISION = minimumRevision;
            IHP_MIGRATION_DIR = "${migrationFiles}/Application/Migration/";
        };
        wantedBy = [ "multi-user.target" ];
        after = [ "postgresql.service" ];
    };

    worker-service = databaseUrl: {
        enable = true;
        serviceConfig = {
            Type = "simple";
            ExecStart = "${package}/bin/RunJobs";
            Restart = "on-failure";
        };
        environment = ihpEnv // { GHCRTS = "-A1M -N1 -qn1"; DATABASE_URL = databaseUrl; };
        wantedBy = [ "multi-user.target" ];
    };
in
{
    # Main tenant
    systemd.services.myapp-migrate = migrate-service "postgres://myapp:@127.0.0.1/myapp" "0";
    systemd.services.myapp-worker = worker-service "postgres://myapp:@127.0.0.1/myapp";

    # Additional tenants — just one line each
    systemd.services.myapp-migrate-acme = migrate-service "postgres://myapp:@127.0.0.1/myapp_acme" "0";
    systemd.services.myapp-worker-acme = worker-service "postgres://myapp:@127.0.0.1/myapp_acme";
}
```

## CI/CD with GitHub Actions

You can automate testing and deployment with GitHub Actions. Here's a minimal workflow:

```yaml
name: Test and Deploy

on:
  push:
    branches: [ main ]
  pull_request:
    branches: [ main ]

jobs:
  tests:
    name: Test
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - uses: cachix/install-nix-action@v27
    - uses: cachix/cachix-action@v15
      with:
        name: digitallyinduced
        skipPush: true
    - name: Run checks
      run: nix flake check --impure

  deploy:
    name: Deploy
    needs: tests
    if: github.ref == 'refs/heads/main'
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v4
    - name: Setup SSH
      run: |
        mkdir -p ~/.ssh
        echo "${{ secrets.SSH_PRIVATE_KEY }}" > ~/.ssh/id_rsa
        chmod 600 ~/.ssh/id_rsa
        ssh-keyscan -H ${{ secrets.SSH_HOST }} >> ~/.ssh/known_hosts
        echo -e "Host production\n  HostName ${{ secrets.SSH_HOST }}\n  User root\n  IdentityFile ~/.ssh/id_rsa" > ~/.ssh/config
        chmod 600 ~/.ssh/config
    - uses: cachix/install-nix-action@v27
    - uses: cachix/cachix-action@v15
      with:
        name: digitallyinduced
        skipPush: true
    - name: Deploy
      run: nix develop --command deploy-to-nixos production
```

The SSH host name in `~/.ssh/config` must match the `nixosConfigurations` key in your `flake.nix`. Store `SSH_PRIVATE_KEY` and `SSH_HOST` as GitHub repository secrets.

### Deploy Script as a Flake App

For multi-app server setups where you manage the NixOS configuration in a separate repository (not inside the IHP app), you can define a deploy command as a flake app:

```nix
# In your server flake.nix:
{
    outputs = { self, nixpkgs, ... }: {
        nixosConfigurations.production = nixpkgs.lib.nixosSystem {
            system = "aarch64-linux";
            specialArgs = { inherit self; };
            modules = [
                ./hardware-configuration.nix
                ./configuration.nix
                ./postgres.nix
                ./myapp.nix
                ./secondapp.nix
            ];
        };

        apps.aarch64-darwin.default = {
            type = "app";
            program = let pkgs = nixpkgs.legacyPackages.aarch64-darwin; in
                "${pkgs.writeShellApplication {
                    name = "deploy";
                    text = ''
                        set -euo pipefail
                        exec ${pkgs.nixos-rebuild}/bin/nixos-rebuild switch \
                            --use-substitutes \
                            --fast \
                            --flake ${self}#production \
                            --target-host root@production \
                            --build-host root@production
                    '';
                }}/bin/deploy";
        };
    };
}
```

Then deploy with `nix run` from the server repo:

```bash
nix flake update  # Pull latest app versions
nix run           # Deploy to production
```

The `--use-substitutes` flag lets the server pull pre-built packages from binary caches, and `--build-host` builds on the server itself (useful when deploying from a different architecture, e.g. macOS to Linux).

### Off-Site Backup Sync

With `services.postgresqlBackup.enable = true`, NixOS saves daily database dumps to `/var/backup/postgresql/` on the server. To sync these off-site, set up a cron job or script on your local machine:

```bash
#!/bin/bash
rsync -avz --delete root@production:/var/backup/postgresql/ ./backups/
```

## Deploying with Docker

Deploying IHP with docker is a good choice for a professional production setup.

IHP has a first party CLI tool called `ihp-app-to-docker-image` to create Docker images out of your app. This tool is available [with IHP Pro and IHP Business](ihp-pro.html). If you're not on IHP Pro yet, now is a good time to try it out. By switching to Pro, you're supporting the sustainable development of IHP.

### Creating a Docker Image

To create a Docker image, we first need to install [Podman](https://podman.io/), and then run the following command:

```bash
nix build .#unoptimized-docker-image --option sandbox false --extra-experimental-features nix-command --extra-experimental-features flakes

cat result | podman load
```

There's also `.#optimized-docker-image` which compiles your app with GHC's `-O2` optimization level. The optimized build produces faster binaries but takes significantly longer to compile. For getting started and testing your deployment, use the unoptimized image. For production, use the optimized image:

```bash
nix build .#optimized-docker-image --option sandbox false --extra-experimental-features nix-command --extra-experimental-features flakes
```

Running `podman images` you can now see that the image is available:

```bash
$ docker images

REPOSITORY     TAG                                IMAGE ID       CREATED         SIZE
app            g13rks9fb4ik8hnqip2s3ngqq4nq14zw   ffc01de1ec7e   54 years ago    86.6MB
```

The `CREATED` timestamp is showing over 50 years ago as the image is built using nix. For having a totally reproducible build, the timestamp is set to `Jan 1970, 00:00 UTC`.

### Worker Docker Image

If your app uses [background jobs](jobs.html), you'll want to run the job worker in a separate container. IHP provides dedicated worker images for this:

```bash
nix build .#unoptimized-docker-image-worker --option sandbox false --extra-experimental-features nix-command --extra-experimental-features flakes

cat result | podman load
```

The worker image is named `ihp-worker` and runs `RunJobs` as its entrypoint instead of `RunProdServer`.

**Building both images:** Since `nix build` writes its output to the `result` symlink, building the app and worker images one after the other will overwrite the first `result`. Use the `-o` flag to write them to different paths:

```bash
nix build .#unoptimized-docker-image -o result-app
nix build .#unoptimized-docker-image-worker -o result-worker

cat result-app | podman load
cat result-worker | podman load
```

The worker container needs the same env variables as the app container (`DATABASE_URL`, `IHP_SESSION_SECRET`, etc.):

```bash
$ docker run \
    -e 'DATABASE_URL=postgresql://postgres:mysecretpassword@the-hostname/postgres' \
    -e 'IHP_SESSION_SECRET=...' \
    ihp-worker:TAG
```

### Running Migrations Automatically in Docker

Instead of using the default `unoptimized-docker-image` or `optimized-docker-image` flake outputs, you can define a custom Docker image in your `flake.nix` that runs database migrations before starting the server. This is useful when you want a single container that handles both migrations and the web server.

**Note:** This pattern is designed for single-replica deployments. If you run multiple replicas, migrations may race against each other. In that case, run migrations as a separate one-shot container or init container before starting the app replicas.

```nix
packages = {
  docker = pkgs.dockerTools.buildImage {
    name = "myapp";
    tag = "latest";
    config = {
      Env = [
        "IHP_MIGRATION_DIR=${./Application/Migration}/"
        "IHP_REQUEST_LOGGER_IP_ADDR_SOURCE=FromHeader"
        "IHP_ENV=Production"
      ];
      ExposedPorts = { "8000/tcp" = { }; };
      Cmd = let migrate-then-run = pkgs.writeShellScript "start-container" ''
        echo "Checking if there are migrations to be run..."
        ${ihp.apps."${pkgs.system}".migrate.program}
        echo "Launching web server"
        exec ${self'.packages.unoptimized-prod-server}/bin/RunProdServer
      ''; in [ migrate-then-run ];
    };
  };
};
```

Pass `DATABASE_URL`, `IHP_BASEURL`, and other environment variables at runtime via `docker run -e` or your orchestrator's env configuration.

Build and load this image with:

```bash
nix build .#docker --option sandbox false --extra-experimental-features nix-command --extra-experimental-features flakes
cat result | podman load
```

This pattern also integrates better with Docker tooling since you control the image name and tag directly.

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

**As of IHP v0.16 this env variable is automatically set to a unique build hash.**

If you use [`assetPath` helpers](assets.html) in your app, specify the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

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

It's therefore important to set it to the external user-facing web address. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

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

Inside your project directory, build your app using one of these commands:

```bash
# For optimized production builds (recommended for production)
nix build .#optimized-prod-server

# For faster unoptimized builds (useful for testing)
nix build .#unoptimized-prod-server
```

This will trigger a full clean build and place the output at `./result`.

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

JS_FILES += ${IHP}/static/vendor/jquery-4.0.0.slim.min.js
JS_FILES += ${IHP}/static/vendor/timeago.js
JS_FILES += ${IHP}/static/vendor/popper-2.11.6.min.js
JS_FILES += ${IHP}/static/vendor/bootstrap.min.js
JS_FILES += ${IHP}/static/vendor/flatpickr.js
JS_FILES += ${IHP}/static/helpers.js
JS_FILES += ${IHP}/static/vendor/turbo.js
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
        <script src="/vendor/jquery-4.0.0.slim.min.js"></script>
        <script src="/vendor/timeago.js"></script>
        <script src="/vendor/popper-2.11.6.min.js"></script>
        <script src="/vendor/bootstrap.min.js"></script>
        <script src="/vendor/flatpickr.js"></script>
        <script src="/helpers.js"></script>
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

### Failure Notifications via Webhook

Sentry catches application-level exceptions, but if the `app` or `worker` systemd service crashes entirely, you won't get a Sentry report. You can use systemd's `onFailure` mechanism to send a webhook notification (e.g. to Slack, Discord, or PagerDuty) when a service fails.

Add a template service and attach it to your app and worker in `configuration.nix`:

```nix
# Generic webhook notification service (template)
systemd.services."FailureNotification@" = {
    enable = true;
    description = "Notifies about %I failure";
    serviceConfig = {
        Type = "oneshot";
        ExecStart = "/bin/sh -c 'curl -X POST -H \"Content-type: application/json\" --data \"{\\\"text\\\":\\\"Service failed: %I\\\"}\" https://hooks.slack.com/services/YOUR/WEBHOOK/URL'";
    };
    path = with pkgs; [ curl ];
};

# Attach to app and worker services
systemd.services.app.onFailure = [ "FailureNotification@%n.service" ];
systemd.services.worker.onFailure = [ "FailureNotification@%n.service" ];
```

The `%I` in the template service is replaced with the name of the failed service (e.g. `app.service`). Adapt the `curl` command for your webhook provider.

### Scheduled Jobs with systemd Timers

For periodic tasks like cleanup scripts or report generation, use systemd timers. This is useful for IHP scripts defined in `Application/Script/`.

Define a oneshot service for the script and a timer to trigger it:

```nix
# The script to run (built from Application/Script/CleanupExpired.hs)
systemd.services.cleanup-expired = {
    serviceConfig = {
        Type = "oneshot";
        ExecStart = "${config.services.ihp.package}/bin/CleanupExpired";
    };
    environment = {
        DATABASE_URL = config.services.ihp.databaseUrl;
        IHP_ENV = "Production";
    };
};

# Run daily at 1:00 AM
systemd.timers.cleanup-expired = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
        OnCalendar = "*-*-* 1:00:00";
        Unit = "cleanup-expired.service";
    };
};
```

The script binary is available at `${config.services.ihp.package}/bin/ScriptName` after building with `nix build`. Check the timer status with `systemctl list-timers`.

## Building with Nix

You can use `nix build` to make a full build of your IHP app:

```bash
# Build an optimized production binary (recommended for production)
nix build .#optimized-prod-server

# Or build an unoptimized binary (faster builds, useful for testing)
nix build .#unoptimized-prod-server
```

This will build a nix package in the `result` directory that contains the following binaries:

-   `result/bin/RunProdServer`, the binary to start web server
-   `result/bin/RunJobs`, if you're using the IHP job queue, this binary will be the entrypoint for the workers
-   a binary for each script in `Application/Script`, e.g. `result/bin/Welcome` for `Application/Script/Welcome.hs`

The build contains an automatic hash for the `IHP_ASSET_VERSION` env variable, so cache busting should work out of the box.

### Starting the app

After building, you can start your app by running:

```bash
result/bin/RunProdServer
```


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

It's therefore important to set it to the external user-facing web address. E.g. if your IHP app is available at `https://example.com/`, the variable should be set to that:

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

If you use [`assetPath` helpers](assets.html) in your app, specify the `IHP_ASSET_VERSION` env var. Set it e.g. to your commit hash or to the release timestamp.

```bash
$ export IHP_ASSET_VERSION=af5f389ef7a64a04c9fa275111e4739c0d4a78d0
$ ./build/bin/RunProdServer
```

#### `GHCRTS`

IHP by default sets some default values for the GHC/Haskell Runtime System (RTS) which work well in production with high loads, at the cost of using a bit of memory. If you want your IHP deployment to use less RAM on your machine, try different values for the environment variable `GHCRTS`. The default value (set in IHP's [Makefile.dist](https://github.com/digitallyinduced/ihp/blob/master/lib/IHP/Makefile.dist#L86)) is `"-A128M -N3 -qn3 -G4"`. If you want very low memory usage, at the cost of more frequent garbage collection, try `export GHCRTS="-A16M -N"`. A middle ground is IHPCloud's default of `export GHCRTS="-A128M -N3 -qn3 -G4"`.

For explanations of these values, see GHC's [manual on the RTS](https://downloads.haskell.org/ghc/latest/docs/users_guide/runtime_control.html) and on [RTS options for concurrency]( https://downloads.haskell.org/ghc/latest/docs/users_guide/using-concurrent.html#rts-options-for-smp-parallelism). In short, `-A` is the allocation area, `-G` is number of generations, `-qn` is number of threads to use for parallel GC, `-N` is number of threads and `-n` is the memory chunk area.

##### Per-Service RTS Tuning

The app server and the job worker have very different workload profiles and benefit from different RTS settings. The `services.ihp.rtsFlags` option sets the flags for both the `app` and `worker` services, but you can override the worker independently:

- **App server** (many concurrent requests): use a larger allocation area and multiple capabilities, e.g. `-A64m -N4 -H32m`
- **Job worker** (processes one job at a time): use minimal allocation and a single capability, e.g. `-A1M -N1 -qn1`

```nix
# In your configuration.nix:
services.ihp.rtsFlags = "-A64m -N4 -H32m"; # App server flags

# Override just the worker:
systemd.services.worker.environment.GHCRTS = "-A1M -N1 -qn1";
```

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

### Managed Services

When using `ihp.nixosModules.appWithPostgres`, the following systemd services are managed automatically:

- **`app.service`** — The main web server (`RunProdServer`)
- **`worker.service`** — The background job runner (`RunJobs`)
- **`migrate.service`** — Database migrations (runs once on deploy, if `services.ihp.migrations` is set)
- **`app-keygen.service`** — Generates the session secret key file on first boot

Check their status after a deployment:

```bash
systemctl status app
systemctl status worker
systemctl status migrate
journalctl -u app -n 50 --no-pager
```

### Memory Limits and Restart Hardening

By default `services.app` and `services.worker` are configured with `Restart = "always"` but do not set any memory bounds. This means that a memory leak in application code, or a runaway allocation inside a long-running job, can consume all available RAM before the Linux OOM killer intervenes. When that happens, the kernel may kill unrelated processes on the host — for example a colocated PostgreSQL when using `appWithPostgres` — instead of cleanly restarting just the offending unit.

To bound the blast radius, set `MemoryHigh` and `MemoryMax` on the unit. Once the cgroup hits `MemoryMax`, systemd terminates only the processes inside that unit and immediately restarts it, because `Restart = "always"` is already the default:

```nix
systemd.services.app.serviceConfig = {
    MemoryHigh = "1500M";
    MemoryMax = "2G";
    RestartSec = "5s";
};

systemd.services.worker.serviceConfig = {
    MemoryHigh = "1500M";
    MemoryMax = "2G";
    RestartSec = "5s";
};
```

- `MemoryMax` is a hard ceiling. Exceeding it triggers the cgroup OOM killer for that unit only; the `Restart = "always"` policy then brings the unit back up.
- `MemoryHigh` is a soft ceiling. Exceeding it throttles the process and forces aggressive memory reclaim, which often surfaces leaks earlier and helps avoid hitting `MemoryMax` at all.
- `RestartSec` prevents a tight restart loop if the unit crashes immediately on startup.

Setting these on `services.worker` is especially valuable: job handlers that process unbounded event histories, call external APIs that return large responses, or build up large in-memory data structures are the most common source of memory growth in a long-lived Haskell process. Because the worker runs in its own systemd unit (and therefore its own cgroup), capping its memory there prevents a leaking background job from taking down the web tier.

Choose values based on your instance size and what else runs on the host. Leave headroom for the kernel, for PostgreSQL if it is colocated, and for any other services. On a 2 GB instance running only `app` and `worker`, `MemoryHigh = "1500M"` / `MemoryMax = "2G"` is a reasonable starting point — tune after observing real usage with `systemd-cgtop` and `systemctl status app.service`.


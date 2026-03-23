# Production Checklist

```toc

```

## Introduction

Before deploying your IHP application to production, work through this checklist to verify that your app is secure, performant, and reliable. Each section covers a specific area with concrete steps you can follow.

This guide assumes you have already read the [Deployment](https://ihp.digitallyinduced.com/Guide/deployment.html) guide and have a working deployment setup. Use this checklist as a final review before going live, and revisit it whenever you make significant changes to your application.

## Environment Configuration

IHP uses the `IHP_ENV` environment variable (or the `option` in `Config/Config.hs`) to switch between `Development` and `Production` behavior. In production mode, IHP automatically adjusts static asset caching, logging, error pages, and background workers.

- [ ] **Set `IHP_ENV` to `Production`**: If you are using `deploy-to-nixos`, this is handled automatically by the NixOS module. If deploying manually, set `IHP_ENV=Production` in your environment.

- [ ] **Set `IHP_BASEURL`**: This must be your full production URL including the scheme, e.g. `https://myapp.example.com`. IHP uses this to generate absolute URLs and to determine whether session cookies should be marked as secure (HTTPS-only). If this is not set correctly, links in emails, redirects, and cookie security will be wrong.

- [ ] **Set `DATABASE_URL`**: Point this to your production PostgreSQL database. The format is `postgresql://user:password@host:port/dbname`. When using `deploy-to-nixos` with `appWithPostgres`, this is configured automatically via the `databaseUrl` option in `configuration.nix`.

- [ ] **Set `IHP_SESSION_SECRET`**: Generate a secure random key using `new-session-secret` and set it as the `IHP_SESSION_SECRET` environment variable or via the `sessionSecret` option in your NixOS configuration. Without this, sessions will not work correctly across application restarts. Alternatively, use `IHP_SESSION_SECRET_FILE` to point to a file containing the key.

- [ ] **Set `IHP_ASSET_VERSION`**: Set this to your git commit hash or a build identifier. IHP appends this as a query parameter to static asset URLs for cache busting. Without it, users may see stale CSS or JavaScript after a deployment.

- [ ] **Move all secrets to environment variables**: API keys, SMTP credentials, S3 access keys, and other secrets should be in environment variables or files, never hardcoded in source code. Use the `env` and `envOrDefault` functions in `Config/Config.hs` to read them. See the [Config guide](https://ihp.digitallyinduced.com/Guide/config.html) for details.

- [ ] **Use a `.env` file only for development**: The `.env` file is loaded by `direnv` in development and should be in your `.gitignore`. In production, set environment variables through your deployment configuration (e.g. `services.ihp.additionalEnvVars` in NixOS).

## Database

- [ ] **Run all migrations**: Use the `migrate` command to apply pending migrations to your production database. The `deploy-to-nixos` tool runs migrations automatically during deployment. Verify that the `schema_migrations` table is up to date.

- [ ] **Set up database backups**: If you are using AWS RDS, enable automated backups with an appropriate retention period. If you are running PostgreSQL on the same server (via `appWithPostgres`), enable backups in your NixOS configuration:

    ```nix
    services.postgresqlBackup.enable = true;
    ```

- [ ] **Configure connection pooling**: IHP uses two connection pools (postgresql-simple and hasql). The hasql pool can be tuned with `HASQL_POOL_SIZE` (default: 3) and `HASQL_IDLE_TIME` (default: 600 seconds). For most applications the defaults are fine, but high-traffic apps may need adjustment. See the [Config guide](https://ihp.digitallyinduced.com/Guide/config.html#database-connection-pool) for details.

- [ ] **Test the production database connection**: Before directing traffic to the new deployment, verify that your app can connect to the production database and that queries return expected results.

## Security

- [ ] **HTTPS is configured and working**: When deploying with `deploy-to-nixos`, the NixOS module sets up Let's Encrypt certificates and forces HTTPS via nginx. Verify that your domain redirects HTTP to HTTPS. IHP automatically marks session cookies as secure when `IHP_BASEURL` starts with `https://`.

- [ ] **Session secret is set and unique**: A missing or default session secret means sessions are not secure. Generate one with `new-session-secret` and keep it confidential. Rotate it if you suspect it has been compromised (this will invalidate all existing sessions).

- [ ] **No debug output in production**: In `Production` mode, IHP automatically hides detailed error pages and backtraces. Verify that `IHP_ENV` is set to `Production` so that users do not see stack traces or internal error details.

- [ ] **File uploads are validated**: If your app accepts file uploads, validate file types and sizes. IHP provides `parseRequestBodyOptions` in the framework config to set limits on request body size, file count, and file size. See the [WAI documentation](https://hackage.haskell.org/package/wai-extra/docs/Network-Wai-Parse.html) for available options.

- [ ] **Firewall is configured**: Allow only necessary ports. A typical configuration:

    ```nix
    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 80 443 22 ];
    ```

- [ ] **SSH uses key-based authentication only**:

    ```nix
    services.openssh.enable = true;
    services.openssh.settings.PasswordAuthentication = false;
    ```

- [ ] **Review the [Security guide](https://ihp.digitallyinduced.com/Guide/security.html)**: IHP provides automatic protection against XSS (via HSX escaping), SQL injection (via parameterized queries), and CSRF (via session-based tokens). Make sure you are not bypassing these protections (e.g. using `preEscapedToHtml` with user input).

## Logging

IHP's default production logger uses `Info` level with Apache-style request logging. You can customize this in `Config/Config.hs`.

- [ ] **Set an appropriate log level**: For production, `Info` or `Warn` is recommended. Avoid `Debug` in production as it generates excessive output and may include sensitive data.

    ```haskell
    -- Config/Config.hs
    import IHP.Log.Types

    config :: ConfigBuilder
    config = do
        logger <- liftIO $ newLogger def { level = Info }
        option logger
    ```

- [ ] **Set up log aggregation**: If you are deploying on AWS, consider forwarding logs to CloudWatch using Vector. The deployment guide includes a complete [CloudWatch configuration](https://ihp.digitallyinduced.com/Guide/deployment.html). On other platforms, you can log to a file with rotation:

    ```haskell
    logger <- liftIO $ newLogger def {
        level = Info,
        destination = File "Log/production.log" (SizeRotate (Bytes (4 * 1024 * 1024)) 7) defaultBufSize
    }
    option logger
    ```

- [ ] **Verify logs do not contain sensitive data**: Check that log messages do not include passwords, session tokens, API keys, or other secrets. Be cautious with `Log.debug` calls that may dump request bodies.

- [ ] **Monitor application logs after deployment**: Check `journalctl --unit=app.service -n 100 --no-pager` on your server to verify the app started cleanly and is handling requests without unexpected errors.

## Static Assets and Performance

- [ ] **Use `assetPath` for all static assets**: The `assetPath` helper prepends `/static/` and appends a version hash for cache busting. Requests to `/static/*` bypass the full middleware stack (session, CORS, etc.) and are served directly by the static file server, which is significantly faster.

    ```haskell
    [hsx|
        <script src={assetPath "/app.js"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]
    ```

- [ ] **Static assets are cached in production**: IHP automatically sets `Cache-Control: max-age=forever` for static files in production mode (and `max-age=0` in development). Make sure `IHP_ASSET_VERSION` is set so that cache busting works on each deployment.

- [ ] **Consider enabling response compression**: Add gzip and/or brotli compression middleware to reduce bandwidth. See the [compression middleware section](https://ihp.digitallyinduced.com/Guide/config.html#compression-middleware) in the Config guide.

- [ ] **Check for N+1 queries**: Review your controller actions for patterns where you fetch a list of records and then query related records one-by-one in a loop. Use `fetch` with `include` or batch queries instead.

- [ ] **Build with optimizations**: The `nix build .#optimized-prod-server` target compiles your app with GHC optimizations enabled. The `unoptimized-prod-server` target skips optimizations for faster build times but slower runtime performance.

## Error Handling

- [ ] **Custom error pages are in place**: In production mode, IHP shows generic error pages without implementation details. You can customize error handling by setting a custom `ExceptionTracker`:

    ```haskell
    -- Config/Config.hs
    import qualified Network.Wai.Handler.Warp as Warp

    config :: ConfigBuilder
    config = do
        option $ ExceptionTracker myExceptionHandler

    myExceptionHandler :: Maybe Request -> SomeException -> IO ()
    myExceptionHandler request exception = do
        -- Log to your error tracking service (e.g. Sentry)
        putStrLn ("Error: " <> show exception)
        Warp.defaultOnException request exception
    ```

- [ ] **Error monitoring is configured**: Consider integrating an error tracking service (such as Sentry or Honeybadger) via a custom `ExceptionTracker`. This gives you alerts when exceptions occur in production rather than discovering them in log files.

## Background Jobs

If your application uses IHP's built-in job system:

- [ ] **The `RunJobs` worker is deployed and running**: In development mode, IHP starts job workers automatically with the dev server. In production, you need to run the `RunJobs` binary separately. The `deploy-to-nixos` NixOS module starts a `worker.service` automatically.

- [ ] **Job retry behavior is configured**: By default, failed jobs are retried up to 10 times with a 30-second delay. Customize `maxAttempts` and `backoffStrategy` per job type if needed:

    ```haskell
    instance Job EmailCustomersJob where
        perform EmailCustomersJob { .. } = do
            -- job logic
        maxAttempts = 3
        backoffStrategy = ExponentialBackoff { delayInSeconds = 5 }
    ```

- [ ] **Job timeouts are set for long-running jobs**: If a job might hang, set `timeoutInMicroseconds` to prevent it from blocking the worker indefinitely.

- [ ] **Monitor the jobs table**: Check the `status` and `last_error` columns of your job tables to catch persistent failures. Consider adding an alert if jobs remain in a failed state.

## DNS and Domain

- [ ] **Domain points to your server**: Verify that your domain's DNS A record (or CNAME) points to your production server's IP address.

- [ ] **SSL certificate is issued and valid**: If using Let's Encrypt via the NixOS module, certificates are obtained and renewed automatically. Verify with `curl -vI https://yourdomain.com` that the certificate is valid and not expired.

- [ ] **www redirect is configured** (if applicable): If you want `www.example.com` to redirect to `example.com` (or vice versa), add a redirect in your nginx configuration:

    ```nix
    services.nginx.virtualHosts."www.example.com" = {
        enableACME = true;
        forceSSL = true;
        globalRedirect = "example.com";
    };
    ```

## Monitoring

- [ ] **Health check endpoint is available**: When using systemd integration (`IHP_SYSTEMD=true`), IHP exposes a `/_healthz` endpoint automatically via the `wai-middleware-health-check` package. You can use this for load balancer health checks and uptime monitoring.

- [ ] **Uptime monitoring is configured**: Use an external monitoring service (such as UptimeRobot, Pingdom, or a simple cron-based curl check) to monitor your health check endpoint. This ensures you are notified quickly if your app goes down.

- [ ] **Server resources are monitored**: Keep an eye on disk space, memory, and CPU usage on your production server. NixOS can accumulate old generations that consume disk space; configure automatic garbage collection:

    ```nix
    nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 7d";
    };
    ```

- [ ] **Database monitoring is in place**: Monitor PostgreSQL connection count, query performance, and disk usage. If using RDS, enable Enhanced Monitoring and set up CloudWatch alarms for high CPU, low storage, and connection count thresholds.

## Graceful Deployments

- [ ] **Warp graceful shutdown is configured**: IHP sets a 6-second graceful shutdown timeout in production mode by default. This means in-flight requests have 6 seconds to complete before the server shuts down. This is handled automatically and does not require configuration.

- [ ] **Nginx retry on 502 during deploys**: During a deployment, there is a brief window where the app is restarting and nginx returns 502 errors. Configure nginx to retry automatically so users do not see errors:

    ```nix
    services.nginx.virtualHosts."example.com".locations."/" = {
        proxyPass = "http://localhost:8000";
        proxyWebsockets = true;
        extraConfig = ''
            recursive_error_pages on;
            proxy_intercept_errors on;
            error_page 502 = @retry;
        '';
    };
    services.nginx.virtualHosts."example.com".locations."@retry" = {
        proxyPass = "http://localhost:8000";
        extraConfig = ''
            proxy_connect_timeout 2s;
        '';
    };
    ```

## Quick Reference

Here is a summary of the essential environment variables for production:

| Variable | Required | Example | Purpose |
|----------|----------|---------|---------|
| `IHP_ENV` | Yes | `Production` | Switches IHP to production behavior |
| `IHP_BASEURL` | Yes | `https://myapp.example.com` | Base URL for links and cookie security |
| `DATABASE_URL` | Yes | `postgresql://user:pass@host/db` | Production database connection |
| `IHP_SESSION_SECRET` | Yes | (output of `new-session-secret`) | Encrypts session cookies |
| `IHP_ASSET_VERSION` | Recommended | `abc1234` | Cache busting for static assets |
| `PORT` | Optional | `8000` | HTTP port (default: 8000) |
| `IHP_REQUEST_LOGGER_IP_ADDR_SOURCE` | Optional | `FromHeader` | Use `FromHeader` behind a reverse proxy |
| `HASQL_POOL_SIZE` | Optional | `3` | Hasql connection pool size |

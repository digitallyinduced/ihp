<p align="center">
  <a href="https://ihp.digitallyinduced.com/" target="_blank">
    <img src="Guide/images/ihp-logo-readme.svg" />
  </a>
</p>

<p align="center">
  <img alt="MIT License" src="https://img.shields.io/github/license/digitallyinduced/ihp">
</p>

<p align="center">
  <a href="https://ihp.digitallyinduced.com/" target="_blank">
    IHP Website
  </a>
</p>

# IHP: The Type-safe Web Framework for Builders

IHP is a batteries-included web framework optimized for longterm productivity and programmer happiness. Built on top of Haskell and Nix. Move fast, without breaking things.

In production since 2017. Used by teams building serious web applications.

[![Watch the IHP introduction](https://img.youtube.com/vi/MiMwZCRCIhk/maxresdefault.jpg)](https://www.youtube.com/watch?v=MiMwZCRCIhk)

## What IHP Code Looks Like

```haskell
-- Controller: Create a new post
action CreatePostAction = do
    let post = newRecord @Post
    post
        |> fill @'["title", "body"]
        |> validateField #title nonEmpty
        |> validateField #body nonEmpty
        |> ifValid \case
            Left post -> render NewView { .. }
            Right post -> do
                post <- post |> createRecord
                redirectTo PostsAction

-- View: Type-safe HTML with HSX
renderPost :: Post -> Html
renderPost post = [hsx|
    <article>
        <h2>{post.title}</h2>
        <p>{post.body}</p>
    </article>
|]
```

## Features

**Type Safety** -- Type-safe routing, queries, and HTML (HSX). Catch errors at compile time.

**AI-Driven Development** -- Optimized for Claude Code and AI-assisted workflows. The type system acts as a safety net -- AI generates code, the compiler verifies it. Ship features faster with confidence. IHP ships with a comprehensive `CLAUDE.md` that teaches AI tools the framework conventions.

**Schema Designer** -- Visual database design tool. Edit tables in a GUI or write SQL directly.

**Code Generators** -- Generate controllers, views, and migrations from the IDE.

**Live Reload** -- Instant feedback during development despite being a compiled language.

![Live Reload](https://github.com/digitallyinduced/ihp/blob/master/Guide/images/IHP%20Live%20Reloading%20Demo.gif?raw=true)

**Auto Refresh** -- Real-time views that update when data changes. One line of code.

![Auto Refresh](https://github.com/digitallyinduced/ihp/blob/master/Guide/images/IHP%20Cloud%20Auto%20Refresh.gif?raw=true)

**Authentication** -- Built-in user signup and login.

**Forms & Validation** -- Type-safe form handling with built-in validation.

**Managed Environment** -- Nix handles all dependencies including PostgreSQL and GHC.

## Quick Start

```bash
nix profile install nixpkgs#ihp-new
ihp-new myproject
cd myproject && devenv up
```

[Full Installation Guide](https://ihp.digitallyinduced.com/Guide/installation.html) | [Your First Project](https://ihp.digitallyinduced.com/Guide/)

## Package Ecosystem

IHP is a monorepo with focused packages:

| Package | Purpose |
|---------|---------|
| `ihp` | Core web framework (routing, controllers, views, models, validation) |
| `ihp-hsx` | HSX templating (JSX-like HTML in Haskell) |
| `ihp-ide` | Dev server, schema designer, code generators |
| `ihp-datasync` | Real-time data sync via WebSockets |
| `ihp-graphql` | GraphQL API |
| `ihp-openai` | OpenAI integration |
| `ihp-ssc` | Server-side components |
| `ihp-hspec` | Testing utilities |
| `ihp-migrate` | Database migration tool |
| `ihp-mail` | Email support |
| `ihp-job-dashboard` | Background job monitoring |
| `ihp-pglistener` | PostgreSQL LISTEN/NOTIFY |

## Deployment

IHP apps deploy to NixOS servers using the built-in `deploy-to-nixos` tool. Your entire server configuration -- nginx, TLS via Let's Encrypt, PostgreSQL, systemd services -- lives declaratively in your git repository under `Config/nix/hosts/production/`.

```bash
deploy-to-nixos production
```

This runs `nixos-rebuild` over SSH to apply your configuration. Systemd socket activation queues requests during deploys for zero-downtime restarts, and a watchdog automatically recovers unresponsive processes.

Docker and bare-metal deployments are also supported. [Full deployment guide](https://ihp.digitallyinduced.com/Guide/deployment.html)

## Community

- [Slack](https://ihp.digitallyinduced.com/Slack) -- Questions, help with type errors, or just chat
- [Forum](https://discuss.ihp.digitallyinduced.com/) -- Longer discussions and announcements

## Contributing

We welcome pull requests! See [CONTRIBUTING.md](CONTRIBUTING.md) for details. Documentation lives in the [Guide directory](https://github.com/digitallyinduced/ihp/tree/master/Guide).


<p align="center">
  <a href="https://ihp.digitallyinduced.com/" target="_blank">
    <img src="https://github.com/digitallyinduced/ihp/blob/master/Guide/images/ihp-logo-readme.png?raw=true" width="420"/>
  </a>
</p>

<p align="center">  
  <img alt="MIT License" src="https://img.shields.io/github/license/digitallyinduced/ihp">
  
  <a href="https://gitter.im/digitallyinduced/ihp" target="_blank">
    <img alt="Gitter" src="https://img.shields.io/gitter/room/digitallyinduced/ihp">
  </a>
  
  <a href="https://twitter.com/digitallyinduce" target="_blank">
    <img src="https://img.shields.io/twitter/follow/digitallyinduce"/>
  </a>
</p>

<p align="center">
  <a href="https://ihp.digitallyinduced.com/" target="_blank">
    IHP Website
  </a>
</p>

# About IHP

IHP is a modern batteries-included haskell web framework, built on top of Haskell and Nix.

We believe that functional programing is the future of software development and want to make functional programing with haskell and nix available to anyone. We try to offer a solution which can be used by developers who have not worked with haskell yet. IHP comes with everything you need to build great web applications with haskell and nix. We have made a lot of pragmatic decision to get you started faster. This way you can just pick up haskell along the way :-)

IHP stands for Integrated Haskell Platform.


## What makes it different?

**Type-safe and reliable:**
With Haskell and Nix we use the most reliable technologies available, to make sure your application will never crash because of Null Pointer Exceptions.

If you like TypeScript, you will love IHP.

**Live Reload:**
While haskell is a compiled language, the built-in dev server automatically reloads your code changes using the fastest way possible. Changes are reflected instantly. Just like good old PHP.

![Live Reload](https://github.com/digitallyinduced/ihp/blob/master/Guide/images/IHP%20Live%20Reloading%20Demo.gif?raw=true)

[Watch it in action!](https://youtu.be/nTjjDo57B8g)

**Accessible:**
Setup of the fully-managed dev environment takes just 5 minutes. All dependencies (even database and compiler) are managed using the nix package manager. This means dependency problems just cannot occur anymore. Also everything is guaranteed to be same for all developers in your team.

**No Haskell Experience required:**
Code Generators will help you to quickly build things even when you have no professional haskell experience yet. Pick up haskell by building real world applications.

**Integrated Dev Tooling:**
To speed up your development process, IHP comes with a full set of web based dev tools. Including: a database schema designer, a web-based code generator, a web-based repl, ...

**Major Operating Systems Supported:**
Windows (via Linux Subsystem), macOS, NixOS, Debian, Ubuntu

**HSX:**
Like React's JSX. Write html code in your haskell files. This will be transformed to actual type-checked haskell code at compile time.

**Auto Refresh:**
Re-render views in the background when the underlying data changes. This is useful when you want your views to always reflect the live database state. Auto Refresh can be an easy replacement for manually polling for changes using AJAX.

Here's a view using Auto Refresh, there's no app specific JS code here. All view updates are triggered by the server:

![Auto Refresh](https://github.com/digitallyinduced/ihp/blob/master/Guide/images/IHP%20Cloud%20Auto%20Refresh.gif?raw=true)

Auto Refresh can be enabled for IHP views with a single line of code.

[Watch it in action!](https://twitter.com/digitallyinduce/status/1312017800223956992)


**Longterm Roadmap**
Lot's of frameworks are already gone a year after launch. Especially in the fast moving JS world. But don't worry about IHP. We have been using it at digitally induced since 2017. It's actively used by us and our friends and partners. Even without external contributors we will build new features and do periodic maintenance releases in the future. We have big plans for IHP and as a profitable and independent software company we have the ability to actually execute them over the longterm.

## Reviews

Here's what other people are saying about IHP:

<blockquote>
IHP is the best web framework experience I have had. I think a lot of aspiring haskeller's will have worked their way through a text like Learn You a Haskell For Great Good or similar and then be looking to fool around with an actual project.
IHP uses nix to cut out all the work of setting up ghc/stack/cabal which can be quite tricky for a beginner, it has just added haskell language server integration, it starts your postgres servers, it spares you from having to remember your SQL syntax, and means you can start writing haskell code instantly while getting immediate feedback on your hacking from GHCi and visually through the live reloading of the web app in your browser.

I've found the [hsx||] quasiquotes quite fun to work with and type safety has sped up development by preventing bugs that can easily crop up when you're linking forms to databases.

I like the design choices in IHP, and for me, at the moment, I like the focus on server side web app development, which encourages you to be more sparing/judicious in the use of javascript.

With IHP being opinionated about the models or views and routing it means I can focus on integrating some of the other really cool haskell libraries that exist into web apps. (I'm also trying to keep a log of my experiences here on an https://ihpcafe.ihpapp.com if you want to read along with someone figuring out function type signatures ;) )
</blockquote>

<a href="https://news.ycombinator.com/item?id=24817368">montmorency88 on HN</a>



<blockquote>
I have been learning Haskell through IHP. It's the best and actually simplest web framework I have ever tried.
The developer experience is the best I ever had for a MVC-style framework. And I got to learn Haskell without banging my head too much against the wall. Really enjoyed it :)

I really needed something like IHP to get started with Haskell. It's fun and easy and the documentation is very easy to follow.

I see some argue that they wish to make more custom choices regarding the setup. That is precisely what I don't want and that's why IHP is a great fit for me.

In terms of simplicity I think the choice of supporting only Nix and Postgres actually are strengths at this point. That lets them streamline the development of the framework without supporting the minor preferences of every individual. And these choices should work really well for most web developers.

Some may disagree on this, and maybe IHP is not for them, but I don't think it shouldn't discourage those who considers trying it out and form their own opinion.

I was prepared for great hardships learning Haskell as most of the resources are very academic and I'm mainly just a simple application builder. IHP was just what I needed to fill that gap, and it made it really fun :)

I am tweeting about the whole thing, coding Haskell for 100 days: https://twitter.com/larsparsfromage
</blockquote>

<a href="https://news.ycombinator.com/item?id=24829215">kodefant on HN</a>

<blockquote>
IHP is supposed to become the Django/Rails/Phoenix of Haskell.

I’ve been using Django professionally for since 2013, but have started using IHP a couple of weeks ago. It’s still quite early but with surprisingly few rough edges, i.e. the developer ergonomics are much better than I expected. It has great documentation that is improving rapidly (as opposed to many other Haskell libraries, which provide little more than API docs or even just the typed function definitions) and offers a refreshing take on database management and migrations.

Some of its killer features:

HSX, a JSX-like template language that looks like HTML while providing type safety

Auto live reloading without the need to setup anything

Documentation with examples: it lets you query the database without learning about monads

it defines |> for you ;-)

type-safe, composable SQL queries
</blockquote>

<a href="https://lobste.rs/s/8pieht/ihp_modern_batteries_included_web#c_6lroyn">hendi on Lobsters</a>

There's more on the IHP website.

## Getting Started

[First: Watch the introduction video to see how it generally works](https://youtu.be/UbDtS_mUMpI)

**After that:**

[If you like videos, check out IHP Casts!](https://www.youtube.com/watch?v=PLl9Sjq6Nzc&list=PLenFm8BWuKlS0IaE31DmKB_PbkMLmwWmG&index=1).

[You can also follow the written Guide to build your first project!](https://ihp.digitallyinduced.com/Guide/) 🚀

[📧 To stay in the loop, subscribe to the IHP release emails.](http://eepurl.com/g51zq1) Or follow [![Follow digitally induced on Twitter](https://img.shields.io/twitter/follow/digitallyinduce)](https://twitter.com/digitallyinduce)

## Community

Questions, or need help with haskell type errors? [Join our Slack Community](https://join.slack.com/t/ihpframework/shared_invite/zt-im0do3yv-iryDNyvpwW~py40kvl_cpQ)

[Also check out the IHP Forum!](https://forum.ihpapp.com/)

## Contributing

We are happy to merge your pull requests!😄

See [CONTRIBUTING.md](CONTRIBUTING.md) for more info.

## IHP Stickers

We're happy to send you Cool & Funky stickers via physical mail. [Get them here](https://forms.gle/hEgV4E7RakQBaKgY9)

## Releases

During beta, there is a new release every two weeks on Friday. You can find steps to update to the latest version [in the release notes](https://github.com/digitallyinduced/ihp/releases).

[📧 Subscribe to the IHP release emails to stay in the loop.](http://eepurl.com/g51zq1)

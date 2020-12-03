# Mail

```toc
```

## Introduction

IHP comes with simple email sending functionality out of the box. It's built on top of the [mime-mail](https://hackage.haskell.org/package/mime-mail) Haskell package.

## Generating Mails

To send a mail we first need to generate a new email template. For that open the Mail Code Generator from the development tools.

![](images/mail/mail-codegen.png)

Inside the code generator, we have to pick a controller. For our example, we want to generate a confirmation mail to send to new users after they sign up. Therefore we select the `Users` controller from the drop-down. We want our mail to be called `Confirmation` mail, so we enter this into the text field:

![](images/mail/mail-codegen-input.png)

Click `Preview` and after that confirm by clicking `Generate`.

This will generate a new email template at `Web/Mail/Users/Confirmation.hs`:

```haskell
module Web.Mail.Users.Confirmation where
import Web.View.Prelude
import IHP.MailPrelude

data ConfirmationMail = ConfirmationMail { user :: User }

instance BuildMail ConfirmationMail where
    subject = "Subject"
    to ConfirmationMail { .. } = Address { addressName = Just "F L", addressEmail = "fname.lname@example.com" }
    from = "hi@example.com"
    html ConfirmationMail { .. } = [hsx|
        Hello World
    |]
```

### Changing Subject

Let's first change the subject of our mail from `Subject` to something more useful:

```haskell
    subject = "Confirm your Account"
```

With this change, the new subject is `Confirm your Account`.

### Changing the Email Receiver

We also need to change the email receiver. It's currently hard-coded to `fname.lname@example.com`. As we want our email sent to the email address of our user, we use the `email` field of the user:

```haskell
to ConfirmationMail { .. } = Address { addressName = Just "F L", addressEmail = get #email user }
```

Because our user has a `name` field, we can also pass this information to our mail, like this:

```haskell
to ConfirmationMail { .. } = Address { addressName = Just (get #name user), addressEmail = get #email user }
```

### Changing the Email Sender

The email sender is set to `hi@example.com` by default. Usually, you want to use your domain here. For this example, we will stick with the `hi@example.com` for now.

### Email Content

Last we need to change the email text a little bit. The mail supports HSX so this is similar to writing a IHP view:

```haskell
    html ConfirmationMail { .. } = [hsx|
        Hey {get #name user}, <br/>
        Thanks for signing up! Please confirm your account by following this link: ... <br /><br />
    |]
```

## Sending Mails

From inside a controller or script, an email can be sent by using `sendMail`:

```haskell
action MyAction = do
    user <- fetch "..."
    sendMail ConfirmationMail { user }
```

## Mail Servers

By default, IHP uses your local `sendmail` to send out the email. IHP also supports sending mail via AWS Simple Email Service (SES). This is recommended for production.

### Custom SMTP Server

Currently using a custom SMTP server is not yet supported. We recommend to use `sendmail` locally (the default) and AWS SES in production.

### AWS SES

To use SES open `Config/Config.hs` and add a `mailServer` function to the construction of `FrameworkConfig`:

```haskell
-- Add this import
import IHP.Mail

config :: ConfigBuilder
config = do
    option Development
    option $ AppHostname "localhost"
    option $ SES
        { accessKey = "YOUR AWS ACCESS KEY"
        , secretKey = "YOUR AWS SECRET KEY"
        , region = "eu-west-1" -- YOUR REGION
        }
```

After that, all emails will be sent through AWS SES.

## Email Attachments

TODO

## Plain Text Emails

TODO

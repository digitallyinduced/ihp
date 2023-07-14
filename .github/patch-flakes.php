<?php
$file_contents = file_get_contents('./flake.nix');
$new_content_with_ref = preg_replace('/(ihp\.url = \")(.+)(\")/', '$1github:digitallyinduced/ihp?ref=' . $argv[1] . '$3', $file_contents);
$new_content_with_ref = str_replace("p.ihp", "p.ihp wreq mmark mmark-ext strip-ansi-escape stripe-signature stripe-concepts http-conduit haskell-to-elm aeson-casing tz tagsoup ihp-zip minio-hs hs-brotli wai-middleware-brotli fakedata jwt ihp-openai ihp-graphql", $new_content_with_ref);
$new_content_with_ref = str_replace("# Native dependencies, e.g. imagemagick", "nodejs imagemagick", $new_content_with_ref);
file_put_contents('./flake.nix', $new_content_with_ref);
?>

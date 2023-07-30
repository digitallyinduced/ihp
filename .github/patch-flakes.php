<?php
$file_contents = file_get_contents('./flake.nix');
$new_content_with_ref = preg_replace('/(ihp\.url = \")(.+)(\")/', '$1github:' . $argv[2] . '?ref=' . $argv[1] . '$3', $file_contents);
$new_content_with_ref = str_replace("p.ihp", "p.ihp wreq mmark mmark-ext strip-ansi-escape stripe-signature stripe-concepts http-conduit haskell-to-elm aeson-casing tz tagsoup ihp-zip minio-hs hs-brotli wai-middleware-brotli fakedata jwt ihp-openai ihp-graphql unordered-containers minimorph", $new_content_with_ref);
$new_content_with_ref = str_replace("# Native dependencies, e.g. imagemagick", "nodejs imagemagick elmPackages.elm", $new_content_with_ref);
file_put_contents('./flake.nix', $new_content_with_ref);
?>

{-# LANGUAGE UndecidableInstances #-}
module IHP.OAuth.Google.View.NewSessionWithGoogle where

import IHP.ViewPrelude
import IHP.OAuth.Google.Types
import qualified IHP.RouterSupport as Router

data NewSessionWithGoogleView = NewSessionWithGoogleView { clientId :: !Text }

instance (Router.HasPath GoogleOAuthController) => View NewSessionWithGoogleView where
    html NewSessionWithGoogleView { .. } = [hsx|
        <button class="btn btn-primary" id="connect-with-google" data-client-id={clientId}>Login with Google</button>
        <script>
            function renderButton() {
                gapi.load('auth2', function() {

                    var clientId = document.querySelector('#connect-with-google').dataset.clientId;
                    auth2 = gapi.auth2.init({ client_id: clientId, scope: 'profile' });

                    const element = document.getElementById('connect-with-google');
                    auth2.attachClickHandler(element, {},
                        function(googleUser) {
                            var form = document.getElementById('new-session-with-google-form');
                            form.querySelector('input[name="jwt"]').value = googleUser.getAuthResponse().id_token;
                            form.submit();
                        }, function(error) {
                            alert(JSON.stringify(error, undefined, 2));
                        });
                });
            }

        </script>

        <script src="https://apis.google.com/js/platform.js?onload=renderButton"></script>

        <form method="POST" action={GoogleConnectCallbackAction} id="new-session-with-google-form">
            <input type="hidden" name="jwt" value=""/>
        </form>

    |]
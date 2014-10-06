lambdatwit
==========

LambdaTwit: Lambdabot running as a twitter bot. Similar to the @fsibot f# bot.


Bot is running at: https://twitter.com/LambdaTwit

##First Run
We need to set the twitter oauth keys into the environment vars like so:
```
$ export OAUTH_CONSUMER_KEY="YOUR APPLICATION CONSUMER KEY"
$ export OAUTH_CONSUMER_SECRET="YOUR APPLICATION CONSUMER SECRET"
$ cabal run oauth_pin

$ export OAUTH_ACCESS_TOKEN="YOUR ACCESS TOKEN"
$ export OAUTH_ACCESS_SECRET="YOUR ACCESS SECRET"
$ cabal run lambdatwit
```


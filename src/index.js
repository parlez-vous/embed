var Elm = require('./elm/Main.elm').Elm;

var apiEndpoint = process.env.API_ENDPOINT

Elm.Main.init({
  node: document.getElementById("parlezvous-comments"),
  flags: {
    apiEndpoint,
  }
});


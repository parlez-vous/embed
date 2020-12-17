var Elm = require('./elm/Main.elm').Elm;

var apiEndpoint = process.env.API_ENDPOINT

var app = Elm.Main.init({
  node: document.getElementById("parlezvous-comments"),
  flags: {
    apiEndpoint,
    siteUrl: document.location.href,
    anonymousUsername: localStorage.getItem('anonymousUsername') 
  }
});


app.ports.writeToLocalStorage.subscribe(([key, value]) => {
  localStorage.setItem(key, value)
})


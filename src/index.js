var Elm = require('./elm/Main.elm').Elm;

var apiEndpoint = process.env.API_ENDPOINT

// for error-reporting purposes
var gitRef = process.env.GIT_REF || null

if (!gitRef && process.env.NODE_ENV === 'production') {
  throw new Error('Missing git ref for production environment')
}

var app = Elm.Main.init({
  node: document.getElementById("parlezvous-comments"),
  flags: {
    apiEndpoint,
    siteUrl: document.location.href,
    anonymousUsername: localStorage.getItem('anonymousUsername'),
    gitRef,
  }
});


app.ports.writeToLocalStorage.subscribe(([key, value]) => {
  localStorage.setItem(key, value)
})


const app = Elm.Main.init({
  flags: {
    storage: JSON.parse(localStorage.getItem('storage')),
    windowHeight: window.innerHeight,
    windowWidth: window.innerWidth
  }
})

app.ports.save.subscribe(storage => {
  localStorage.setItem('storage', JSON.stringify(storage))
  app.ports.load.send(storage)
})

window.addEventListener('storage', function (event) {
  if (event.key === 'storage') {
    app.ports.load.send(JSON.parse(event.newValue));
  }
});

task default: [:serve]

task :serve do |t|
  sh "elm-spa server"
end

task :deploy do |t|
  sh "ps axuww | awk '/nod[e].*elm-spa serve/ { printf \"kill -INT %s\\n\", $2 }' | sh"
  sh "elm-spa build"
  sh "rsync -av --delete-after -e ssh public/ rafc:/var/www/wejn.org/root_odorik/"
end

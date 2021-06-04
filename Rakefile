task default: [:serve]

task :serve do |t|
  sh "elm-spa server"
end

task :deploy do |t|
  sh "ps axuww | awk '/nod[e].*elm-spa serve/ { printf \"kill -INT %s\\n\", $2 }' | sh"
  sh "elm-spa build"
  t = Time.now.to_i
  sh "sed -i 's@\\.js[^\"]*\"@.js?ts=#{t}\"@g' public/index.html"
  sh "rsync -av --delete-after -e ssh public/ rafc:/var/www/wejn.org/root_odorik/"
  sh "sed -i 's@\\.js[^\"]*\"@.js\"@g' public/index.html"
end

sudo sh -c 'echo "deb http://repo.tox.im/deb/ testing main" > /etc/apt/sources.list.d/toxrepo.list'
curl -k https://repo.tox.im/toxbuild.pgp | sudo apt-key add -
sudo apt-get update

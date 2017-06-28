FROM ocaml/opam
RUN sudo apt-get update && opam update && opam depext -i -y ppx_deriving_yojson eliom cryptokit 
EXPOSE 8080
COPY . reference
RUN sudo chown -R opam:opam reference
WORKDIR reference
CMD make test.opt
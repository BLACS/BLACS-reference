FROM ocaml/opam
RUN git clone https://github.com/BLACS/API.git
RUN opam pin add -n blacsapi API/ocaml_api
RUN sudo apt-get update && opam update && opam depext -i -y ppx_deriving_yojson eliom cryptokit blacsapi
EXPOSE 8080
COPY . reference
RUN sudo chown -R opam:opam reference
WORKDIR reference
CMD make test.byte
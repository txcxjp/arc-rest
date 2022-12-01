FROM fedora
RUN yum install -y git oathtool jq 
ENV LANG="C.utf8" \
    LANGUAGE="C.utf8" \
    LC_ALL="C.utf8"
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN git clone https://github.com/txcxjp/arc-rest.git 
RUN git clone https://github.com/thorsten-gehrig/alexa-remote-control.git && mv /alexa-remote-control /usr/lib/alexa-remote-control
RUN cd /arc-rest && stack build --profile
ENTRYPOINT cd /arc-rest && git pull && stack build --profile && stack exec --profile arc-rest-exe -- +RTS -xc

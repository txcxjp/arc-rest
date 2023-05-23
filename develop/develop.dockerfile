FROM fedora
RUN yum install -y git oathtool jq 
ENV LANG="C.utf8" \
    LANGUAGE="C.utf8" \
    LC_ALL="C.utf8"
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN git clone https://github.com/txcxjp/alexa-remote-control.git && mv /alexa-remote-control /usr/lib/alexa-remote-control
# COPY . /arc-rest/
# RUN cd /arc-rest && 
CMD cd /arc-rest &&  stack build --profile --allow-different-user  &&  stack exec --allow-different-user --profile arc-rest -- +RTS -xc

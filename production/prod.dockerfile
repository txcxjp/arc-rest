FROM fedora
RUN yum install -y git oathtool jq 
ENV LANG="C.utf8" \
    LANGUAGE="C.utf8" \
    LC_ALL="C.utf8"
RUN git clone https://github.com/txcxjp/alexa-remote-control.git && mv /alexa-remote-control /usr/lib/alexa-remote-control
RUN mkdir /arc-rest && curl -L -o /usr/bin/arc-rest https://github.com/txcxjp/arc-rest/releases/download/arc-rest-0.0.3/arc-rest && chmod 755 /usr/bin/arc-rest
ENTRYPOINT /usr/bin/arc-rest

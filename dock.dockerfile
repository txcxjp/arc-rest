FROM fedora
RUN yum install -y git oathtool jq
RUN curl -sSL https://get.haskellstack.org/ | sh
ADD "https://www.random.org/cgi-bin/randbyte?nbytes=10&format=h" /dev/null
RUN git clone https://github.com/txcxjp/arc-rest.git 
RUN git clone https://github.com/thorsten-gehrig/alexa-remote-control.git && mv /alexa-remote-control /usr/lib/alexa-remote-control
RUN cd /arc-rest && stack build
ENTRYPOINT cd /arc-rest && git pull && stack run

FROM fedora
RUN yum install -y git
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN git clone https://github.com/txcxjp/arc-rest.git 
RUN git clone https://github.com/thorsten-gehrig/alexa-remote-control.git && mv /alexa-remote-control /usr/lib/alexa-remote-control
RUN cd /arc-rest && stack build
ENTRYPOINT cd /arc-rest && stack run

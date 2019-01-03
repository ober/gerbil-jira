from gerbil/scheme:latest

MAINTAINER jaimef@linbsd.org
COPY . /root/jira
ENV PATH "$PATH:/root/gerbil/bin"
ENV GERBIL_HOME "/root/gerbil"
RUN cd /root/jira && ./build.ss static
RUN cp /root/jira/jira /bin/jira
RUN rm -rf /root/gerbil /root/gambit
CMD /bin/bash

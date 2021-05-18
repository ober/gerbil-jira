from jaimef/alpine-current:static

MAINTAINER jaimef@linbsd.org
COPY . /src
RUN cd /src && rm -rf .gerbil
RUN make -C /src linux-static
RUN mv /src/jira-bin /bin

CMD [ /bin/jira ]

FROM correl/erlang
RUN apt-get update
RUN apt-get install -y git
RUN mkdir -p /root/.ssh
RUN ssh-keyscan github.com >> ~/.ssh/known_hosts
RUN git clone https://github.com/ekiaa/amrd.git; \
    cd amrd; \
    ./rebar get-deps; \
    ./rebar compile; \
    ./rebar generate

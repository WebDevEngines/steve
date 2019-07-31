FROM erlang:alpine

RUN apk add --no-cache make

RUN mkdir -p /code/
COPY . /code/

RUN cd /code/ && make
RUN echo 'cd /code/ && make run' > /bin/start_steve
RUN chmod 777 /bin/start_steve

CMD start_steve
EXPOSE 8093

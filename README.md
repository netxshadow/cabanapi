# erl_template
================================================================================

This project was created for the erlang app template. Connected web server functionality, connection checks(health check), metrics

# QuickStart

```sh
# clone
$ git clone https://github.com/bpotorochyn/erl_template.git
$ cd erl_template

# compile
$ make release

# start
$ ./_build/default/rel/erl_template/bin/erl_template console

```


# Configuring

Configuration exists for 3 kinds of environments:

- local - config/local/sys.config.src
- develop - config/develop/sys.config.src
- production - config/production/sys.config.src

# Health check

You can check the health of the application by sending a GET request: 

curl ${base_url}/api/health_check where base_url - application url



Response must be of type JSON:

{"result":"ok"}# cabanapi
# cabanapi
# cabanapi
# cabanapi

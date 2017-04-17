set +e

STACK_YAML=stack-7.10.3.yaml stack build  --no-terminal
STACK_YAML=stack-8.0.1.yaml  stack build  --no-terminal
stack build --no-terminal

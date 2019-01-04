$(eval squid_ip := $(shell docker inspect squid|jq -r '.[].NetworkSettings.IPAddress'))

docker:
	docker build --build-arg squid=$(squid_ip) --rm=true -t jira .
	docker tag jira jaimef/jira

push:
	docker push jaimef/jira

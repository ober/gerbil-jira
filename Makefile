docker:
	docker build --rm=true -t jira .
	docker tag jira jaimef/jira

push:
	docker push jaimef/jira

#!/bin/bash

yesod keter

scp -i ~/Downloads/admin.pem /home/kevin/workspace/data-scout/data-dictionary.keter ubuntu@$1:/opt/keter/incoming/

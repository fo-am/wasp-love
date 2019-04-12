cp crabs.db crabs-bak.db
gzip -c crabs-bak.db | uuencode crabs-bak.db.gz  | mail -s "crab backup" nebogeo@gmail.com

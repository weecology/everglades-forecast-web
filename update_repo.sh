#!/bin/bash
set -e
LOG_FILE="/pgsql/everglades-forecast-web/cron.log"
echo "$(date) - Starting repository update" > "$LOG_FILE"
cd /pgsql/everglades-forecast-web/everglades-forecast-web
echo "Checking for updates from remote..." >> "$LOG_FILE"
git remote update

# Check if the branch is explicitly up-to-date
if git status -uno | grep -q "Your branch is up to date with 'origin/main'"; then
  echo "Repository is already up-to-date $(date)" >> "$LOG_FILE"
else
  echo "Repository needs updating, resetting to match origin/main..." >> "$LOG_FILE"
  # Force reset to match origin/main exactly
  git fetch origin
  git reset --hard origin/main
  echo "Updated to latest main branch $(date)" >> "$LOG_FILE"
fi

echo "$(date) - Everglades forecast repo update completed" >> "$LOG_FILE"

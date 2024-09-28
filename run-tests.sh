#!/usr/bin/env bash
AWS_ACCESS_KEY_ID=minioadmin AWS_SECRET_ACCESS_KEY=minioadmin cabal run tests -- --qc-max-shrinks=10 --qc-max-success=5 --qc-max-size=5

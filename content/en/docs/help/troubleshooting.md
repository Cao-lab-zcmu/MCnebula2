---
date: "2020-11-12T15:22:20+01:00"
description: Solutions to common problems.
draft: false
images: []
lastmod: "2020-11-12T15:22:20+01:00"
lead: Solutions to common problems.
menu:
  docs:
    parent: help
title: Troubleshooting
toc: true
weight: 620
---

## Problems updating npm packages

Delete the `./node_modules` folder, and run again:

```bash
npm install
```

## Problems with cache

Delete the temporary directories:

```bash
npm run clean
```

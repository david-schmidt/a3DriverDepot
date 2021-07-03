# Build System

### Json database

```
  {
    "driver_dib1": ".VSDRIVE",
    "description": "Apple /// Virtual Serial Drive Driver by David Schmidt 2012 - 2014",
    "dir": "vsdrive",
    "repo": "https://github.com/ADTPro/adtpro",
    "asm": "blob/main/src/client/sos/serial/drive/vsdrive.asm",
    "sha": "552170569181b05e810bdbcca6828efab3b3ae83",
    "local_asm": "vsdrive.asm"
  }
```
`"repo`: The GitHub repo, or other source of truth for this driver   
`"asm"`: The upstream source of the main assembly file (or potentially disk image), concatenated with `repo` to arrive at upstream source of truth   
`"sha`: If from a GitHub repo, the sha of the commit resulting in the upstream source   
`"local_asm"`: The downstream ca65-assembly version of `"asm"`, if it wasn't already in `ca65` form

# Build System

### Driver manifest
`manifest.json`:

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

### Todo

Build or borrow a container that includes:
 * ca65 from [cc65](https://github.com/cc65/cc65)
 * python 2.x ([a3drivertuil](https://github.com/robjustice/a3driverutil) is in python 2)
 * Java ([AppleCommander](https://github.com/AppleCommander/AppleCommander) is in Java)

The build process is then to read the manifest, visit each `dir` referenced, assemble and driver-ize the source `local_asm`, potentially gathering all into an output directory or adding them to disk images (there's only so many that would fit on a single 5-1/4" disk image).
The name of the driver file itself would be the first part of the `local_asm` name, in all capital letters, concatenated with `.DRVR`.
It is expected that a service such as GitHub actions could be used to run the container and drive the build process, exporting artifacts to be consumed.
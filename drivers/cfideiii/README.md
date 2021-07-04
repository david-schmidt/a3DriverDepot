# Apple /// CF/IDE driver

Features:   
 * Native Apple /// SOS driver written in 6502 code.
 * Up to 8 user definable partitions - up to 16 mb each.
 * Can manage up to 2 devices - master & slave.

Master (device 0) can be either Compactflash memory installed in the built-in
socket on the interface card or a IDE HDD attached to the 40pin IDE connector
and configured as master.

Slave (device 1) can be either Compactflash (you will need an adapter that
can be connected to a 40pin IDE connector and slave configurable) or an IDE HDD
configured as slave.

Although any size of device can be attached and successfully used, the
maximum usable capacity is limited to 16,777,215 blocks (8 gig).  Each device is
subdivided into 262,144 block partition maps and depending on the size of the
drive, 64 independent partition tables are possible.  For example, a device
with a capacity of 2,822,400 blocks (1.4 gig) will have 11 partition maps. The
the first ten partitions (#0 thru 9) are 262,144 blocks long and the 11th (#10)
partition is 200,960 blocks long.

### Driver Configuration Block (DCB):

```
Bytes:    00    01    02
         [Md]  [0p]  [0t]
                     d = Device Number, 0 for master, 1 for slave
                     M = Partition map number, 0 to 63 depending on drive size

                     p = Partition Number, 0-7.  This is the partition on the
                         device to use.
```

DIB Values for Map & Drive No.

```
 Map No.     IDE $0     IDE $1        Map No.     IDE $0     IDE $1
    0          $00        $01           32         $80         $81
    1          $04        $05           33         $84         $85
    2          $08        $09           34         $88         $89
    3          $0C        $0D           35         $8C         $8D
    4          $10        $11           36         $90         $91
    5          $14        $15           37         $94         $95
    6          $18        $19           38         $98         $99
    7          $1C        $1D           39         $9C         $9D
    8          $20        $21           40         $A0         $A1
    9          $24        $25           41         $A4         $A5
   10          $28        $29           42         $A8         $A9
   11          $2C        $2D           43         $AC         $AD
   12          $30        $31           44         $B0         $B1
   13          $34        $35           45         $B4         $B5
   14          $38        $39           46         $B8         $B9
   15          $3C        $3D           47         $BC         $BD
   16          $40        $41           48         $C0         $C1
   17          $44        $45           49         $C4         $C5
   18          $48        $49           50         $C8         $C9
   19          $4C        $4D           51         $CC         $CD
   20          $50        $51           52         $D0         $D1
   21          $54        $55           53         $D4         $D5
   22          $58        $59           54         $D8         $D9
   23          $5C        $5D           55         $DC         $DD
   24          $60        $61           56         $E0         $E1
   25          $64        $65           57         $E4         $E5
   26          $68        $69           58         $E8         $E9
   27          $6C        $6D           59         $EC         $ED
   28          $70        $71           60         $F0         $F1
   29          $74        $75           61         $F4         $F5
   30          $78        $79           62         $F8         $F9
   31          $7C        $7D           63         $FC         $FD
```

### CF-IDE Status Requests

The following list gives the status code and the contents of the status list for
each status request supported.

Status Code: $00 (Device Status)
Status List: Device Status ($01 bytes)

Always returns $00 for SOS device status

Status Code: $01 (Device Identification)
Status List: $0200 Data Buffer

Copies the device identification data block into the status list data buffer.
This buffer must be 512 bytes long. Refer to the CF/ATA interface specifications
for the format and content of the device identification data.

Status Code: $02 (Device Error Status)
Status List: Error Data Buffer  ($05 bytes)

Returns 5 bytes of error data from the most recent read/write command that
failed.  The format of the error data is as follows:

```
   Byte 0:      Device Error Code
      Bit 7 = BBK bad block has been detected
      Bit 6 = UNC uncorrectable error was encountered
      Bit 5 = 0 (undefined)
      Bit 4 = IDNF requested block ID is in error or cannot be found
      Bit 3 = 0 (undefined)
      Bit 2 = ABRT command was aborted or an invalid command
      Bit 1 = 0 (undefined)
      Bit 0 = AMNF general error
   Byte 1,2,3:  Sector Number (LB,MB,HB) that error occurred.
   Byte 4:      Number of blocks left to complete command
```
Status Code: $03 (Partition Table Data)
Status List: Partition Data Buffer ($0100 bytes)

Copies 256 bytes of partition table data to status list buffer.  The format of
the partition table is as follows:

```
   Byte $ 0:  Checksum of partition table bytes $02 through $FF (1 byte)
   Byte $10:  Device serial number (20 ASCII characters)
   Byte $24:  Device firmware revision number (8 ASCII characters)
   Byte $2C:  Device model information (40 ASCII characters)
   Byte $64:  Original partition initialization date (8 ASCII characters)
   Byte $6C:  Date partition was last modified (8 ASCII characters)
   Byte $A3:  Partition #1 Start Block (3 bytes)
   Byte $A6:  Partition #1 Length (3 bytes)
   Byte $A8:  Partition #2 Start Block (3 bytes)
   Byte $AB:  Partition #2 Length (3 bytes)
   Byte $B3:  Partition #3 Start Block (3 bytes)
   Byte $B6:  Partition #3 Length (3 bytes)
   Byte $B8:  Partition #4 Start Block (3 bytes)
   Byte $BB:  Partition #4 Length (3 bytes)
   Byte $C3:  Partition #5 Start Block (3 bytes)
   Byte $C6:  Partition #5 Length (3 bytes)
   Byte $C8:  Partition #6 Start Block (3 bytes)
   Byte $CB:  Partition #6 Length (3 bytes)
   Byte $D3:  Partition #7 Start Block (3 bytes)
   Byte $D6:  Partition #7 Length (3 bytes)
   Byte $D8:  Partition #8 Start Block (3 bytes)
   Byte $DB:  Partition #8 Length (3 bytes)
```

Status Code: $04 (DIB configuration bytes)
Status List: DIB configuration bytes (2 bytes)

```
   Byte $00:  Drive number, Bit 0 = $0 (master) or $1 (slave)
                           ;Upper 5 bits = Partition address.
                           ;32 different partition addresses available
   Byte $01:  Partition number on the drive = $00-07
```
Status Code: $FE (Preferred Bitmap Location)
Status List: Bitmap Initial Block ($02 bytes)

Always returns $FFFF.

### CF-IDE Control Requests
The following list gives the control code and the contents of the control list
for each control request supported.

Control Code: $00 (Reset Device)
Control List: None

Execute a software reset call to all attached AT devices.

Control Code: $01 (Device I/O Function)
Control List: None

Perform device direct I/O function with user supplied call block.  The format
of I/O buffer call block is as follows:

```
    Byte $00:      ATA Command Code
    Byte $01:      Sector Number HB
    Byte $02:      Sector Number MB
    Byte $03:      Sector Number LB
    Byte $04:      Number of sectors ($00=256 sectors)
    Byte $05,$06   Bytes returned to buffer (HB,LB)
    Byte $07...    Data Buffer ($00 to call specific length)
```
Supported ATA command codes:

```
  $03 = Extended Error Info - Returns an extended error code for the compact
        flash memory card.  Data buffer length = 1 byte.
  $20 = Sector Read - Reads from 1 to 256 sectors (blocks) as requested
        beginning with the sector number specified.  Data buffer length = 512
        bytes to 131072 bytes long depending on requested number of sectors.
  $30 = Sector Write - Writes from 1 to 256 sectors (blocks) as requested
        beginning with the sector number specified.  Data buffer length = 512
        bytes to 131072 bytes long depending on requested number of sectors.
  $40 = Read verify - Verifies the number of sectors requested beginning with
        the sector number specified.  Data buffer length = None.
  $50 = Sector Format - Formats the specified sector numbers beginning with
        sector number specified.  Data buffer length = None.
  $90 = Internal Diagnostic Test - Performs the device internal diagnostic test.
        The diagnostic code is returned in Data Buffer.   Data buffer
        length = 1 byte.
  $EC = Device Identity - Transfers 512 bytes of device identification data.
        Format per CF/ATA specs applicable for the device.  Data buffer
        length = 512 bytes.
```
Control Code: $04 (Set DIB configuration bytes)
Control List: DIB configuration bytes (2 bytes)

```
   Byte $00:  Drive number, Bit 0 = $0 (master) or $1 (slave),
              Upper 5 bits = Partition address, 32 different partition addresses
              available (See DIB Values for Map & Drive No. table above)
   Byte $01:  Partition number on the drive = $00-07
```
Control Code: $FE (Media Format)
Control List: None

Invalidates partition table status flag internal to the driver so subsequent
read/writes will update its partition parameters with the current partition
values on the device.  Physically does not perform any format operations.
Use the CF/IDE or System Utilities programs to format the drive or device.

# Apple /// CF/IDE Utilities Program version 1.26
Utility program to setup, test, and manage your CompactFlash or IDE drives
attached to the CFFA interface card.

Features:
1. Can manage up to 2 devices - master & slave for each IFC installed.
2. Test the interface connectivity with the CF or IDE drives attached.
3. Displays identification summary of devices connected - model & serial #,
   firmware version, CF command features & logic block capability, & total
   block capacity of device.
4. Verify the media of each device.
5. Low Level format of devices with CFA features capability and initialize
   partition table.
6. Setup drive partitions, a maximum of 8 drive partitions (up to 16 mb or
   32767 blocks each) per partition table.  Up to 64 partition tables are
   possible depending on the size of the IDE device.
7. Perform SOS system level formatting of each partition volume.

## Main Menu
### LIST DEVICES ONLINE
Scans the SOS driver table and lists the drive name of the 1st SOS unitnum for
each CF/IDE device configured and operational.  If an IDE drive is connected and
operational but is not configured in the driver then it is not listed.  Up to 4
interface cards are supported or a maximum of 12 devices (2 on each IFC).  For
example, if there is only 1 interface card installed with both master
(device #0) and slave (device #1) attached then 2 drives are listed.  All
functions of this utility are applicable to the attached devices and not to the
individual SOS drives that may be configured on each device (a maximum
combination of 8 drives is possible).

### TEST IFC
Commands each device to perform its internal diagnostics.  The diagnostic result
displayed will represent the result from both devices attached.  All non-pass
diagnostic results for the slave device are preceded with "Slave Failed - "
followed by the appropriate diagnostic error reported.  Currently the CFFA card
does not support the "device #1 passed diagnostic signal" from a slave device.
If a slave device is present and both master and slave devices pass the
diagnostics the diagnostic message displayed will always be "Slave Failed - No
Errors Detected"  To receive the correct results you will need to connect the
PDIAG signal from pin 34 at the IDE header to pin 46 at the compactflash socket
with a jumper wire.

Following the internal diagnostic message a summary of the device identification
is displayed for each device configured in the device driver.  If 2 devices are
configured, the master device is displayed first followed by the slave device.
If more than 1 device driver is installed (CFFA cards installed in different
slots) then you will be prompted to select which driver to display device
identification.

The information that is displayed is as follows:
DEVICE NO:  Either 0 (master) or 1 (slave)   
MODEL NO:  40 character model # information on the device   
SERIAL NO: 20 character serial # of the device   
FIRMWARE:  8 character firmware revision number   
LBA SUPPORT: Logical Block Access supported by this device - Yes or No.  This
driver and utility only supports devices with LBA support.   
CFA FEATURE:  Compactflash feature set support by the device - Yes or No.   
If a device supports the CFA feature then low level formatting of the device
media will be permitted.   
CAPACITY:  The maximum number of usable blocks on the device.  If device
capacity exceeds 16777215 blocks then > 16777K is displayed.

### VERIFY DRIVE
A read verify of all usable blocks on the device is performed.  A progress bar
displays the percentage of completion. Any bad blocks reported are displayed
in the active window above the progress bar.  The verification can be
halted/resumed at any point through the progress of the verification process.

### FORMAT DRIVE
OA-F - Low Level Format
If the device supports the CFA Feature Set then a complete low level media
format of all usable block on the device is performed beginning with block $0.
The low level format can be halted/resumed at any point through the progress of
the format process.  If the device does not support the CFA Feature Set
(typical of most IDE HDD) all partition tables on the drive are re-formatted
(block $0 of each partition table segment).

### OA-P - Format a Partition Table
This command will prompt you for a specific partition table number to format
if the size of the drive supports multiple partition tables.

### PARTITION DRIVE
Edit or assign partitions for each partition table segment on the drive.  A
maximum of 8 partitions is allowed on each partition table segment.  This
command will prompt you for the specific partition table number to edit if the
size of the drive supports multiple partition tables.  The information displayed
is an open edit table of partition information - starting block, Partition
Length, & Volume Name along with a header of device information (model #, slot,
drive, capacity), partition initialization date, last modified date, & remaining
unused blocks.
Starting Block:  Acceptable values for starting block is anything from 1 to
262143 or the maximum capacity of the partition segment, whichever is less.
Block 0 is reserved for the partition table.  Entering a zero starting block
will be interpreted by the partition editor to assign the next available
starting block.  Any change to an existing starting block will result in the
erasure of the existing volume and all of its data for that partition.
Partition Length:  Acceptable values for partition length is anything from 0
to 32767 or the remaining capacity of the partition segment, whichever is less.
Any change to an existing partition length results in the erasure of the
existing volume and all of the data for that partition.
Volume Name:  The current SOS formatted volume name on the partition.  If a
volume has not been initialized or a change in either the partition starting
block or lengththen "UNFORMATTED" will appear in the volume name column so long
as the partition length is greater than 7 blocks.  If the partition length is
less than 8 blocks then "Undefined" will appear in the volume name column.
To initialize or reformat the volume of the partition enter open-apple F or type
in a new name for the volume.  This is a high level format routine to initialize
a standard SOS root directory and file control block on the partition.

### CHANGE DRIVER CONFIGURATION
This function allows you to dynamically change the driver configuration byte
parameters for each of the CFide drivers online.  The table of information
presented is the IDE device number (0 or 1), the partition table segment #
(0 thru 63 depending on device capacity) and partition number within the table
segment (0 thru 7).  This is different than the actual configuration bytes as
entered with the Apple /// System Utilities SCP program.  The changes made here
will remain in affect until you reboot which will reload the drivers from the
SOS.DRIVER file.  Permanent changes to the configuration bytes will need to be
done with the Apple /// System Utilities SCP program.

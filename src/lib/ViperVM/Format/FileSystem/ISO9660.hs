{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | ISO 9660 / ECMA-119
--
-- File system used on optical disks (CDROM, DVD, BD) and also on some bootable
-- devices (USB sticks, etc.)
module ViperVM.Format.FileSystem.ISO9660
   ( StringA (..)
   , StringD (..)
   , BothEndian (..)
   , DateTime (..)
   , Header (..)
   , VolumeDescriptorType (..)
   , BootRecord (..)
   , PrimaryVolume (..)
   )
where

-- TODO
-- * ISO9660
-- * RockRidge
-- * Joliet
-- * El-Torito
-- * Boot Information Table (not standardized)
--

import ViperVM.Format.Binary.Vector
import ViperVM.Format.Binary.Enum
import ViperVM.Format.Binary.Endianness
import ViperVM.Format.Binary.Word

import GHC.Generics
import GHC.TypeLits
import Foreign.Ptr
import Foreign.Storable
import Foreign.CStorable
import Foreign.C.Types

-- | String with characters: A-Z 0-9 _ * " % & ' ( ) * + , - . / : ; < = > ?
newtype StringA (n :: Nat) = StringA (Vector n CChar) deriving (Show,Generic,CStorable)

instance (KnownNat n) => Storable (StringA n) where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | String with characters: A-Z 0-9 _
newtype StringD (n :: Nat) = StringD (Vector n CChar) deriving (Show,Generic,CStorable)

instance (KnownNat n) => Storable (StringD n) where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

-- | Store the number in both endiannesses: Little-Endian then Big-Endian
newtype BothEndian w = BothEndian w

-- | Date-time mostly in text format
data DateTime = DateTime
   { dateYear                    :: StringD 4 -- ^ Year from 1 to 9999
   , dateMonth                   :: StringD 2 -- ^ Month from 1 to 12
   , dateDay                     :: StringD 2 -- ^ Day from 1 to 31
   , dateHour                    :: StringD 2 -- ^ Hour from 0 to 23
   , dateMinute                  :: StringD 2 -- ^ Minute from 0 to 59
   , dateSecond                  :: StringD 2 -- ^ Second from 0 to 59
   , dateCentiSeconds            :: StringD 2 -- ^ Hundredths of a second from 0 to 99
   , dateTimeZone                :: Word8     -- ^ Time zone offset from GMT in 15 minute intervals, starting at interval -48 (west) and running up to interval 52 (east). So value 0 indicates interval -48 which equals GMT-12 hours, and value 100 indicates interval 52 which equals GMT+13 hours.
   } deriving (Show,Generic)

deriving instance CStorable DateTime
instance Storable DateTime where
   sizeOf    = cSizeOf
   alignment = cAlignment
   peek      = cPeek
   poke      = cPoke

instance (ByteReversable w, Storable w) => Storable (BothEndian w) where
   sizeOf _              = 2 * (sizeOf (undefined :: w))
   alignment _           = alignment (undefined :: w)
   peek p                = (BothEndian . littleEndianToHost) <$> peek (castPtr p)
   poke p (BothEndian v) = do
      let s = sizeOf (undefined :: w)
      pokeByteOff (castPtr p) 0 (hostToLittleEndian v)
      pokeByteOff (castPtr p) s (hostToBigEndian v)


-- | Volume Descriptor header
data Header = Header
   { headerType       :: EnumField Word8 VolumeDescriptorType -- ^ Volume descriptor type
   , headerIdentifier :: StringA 5                            -- ^ Always "CD001"
   , headerVersion    :: Word8                                -- ^ Volume descriptor version (always 0x01)
   }

-- | Volume descriptor type
data VolumeDescriptorType
   = BootRecordType
   | PrimaryVolumeDescriptorType
   | SupplementaryVolumeDescriptorType
   | VolumePartitionDescriptorType
   | CustomVolumeType Word8
   | VolumeDescSetTerminator
   deriving (Show,Eq)

instance CEnum VolumeDescriptorType where
   fromCEnum = \case
      BootRecordType                       -> 0
      PrimaryVolumeDescriptorType          -> 1
      SupplementaryVolumeDescriptorType    -> 2
      VolumePartitionDescriptorType        -> 3
      CustomVolumeType w                   -> fromIntegral w
      VolumeDescSetTerminator              -> 255
   toCEnum = \case
      0   ->  BootRecordType
      1   ->  PrimaryVolumeDescriptorType
      2   ->  SupplementaryVolumeDescriptorType
      3   ->  VolumePartitionDescriptorType
      255 ->  VolumeDescSetTerminator
      w   ->  CustomVolumeType (fromIntegral w)

-- | Boot Record descriptor
data BootRecord = BootRecord
   { bootSystemIdentifier :: StringA 32        -- ^ ID of the system which can act on and boot the system from the boot record
   , bootIdentifier       :: StringA 32        -- ^ identification of the boot system defined in the rest of this descriptor
   , bootCustomData       :: Vector 1977 Word8 -- ^ Custom - used by the boot system (e.g., El Torito)
   } deriving (Show)

data PrimaryVolume = PrimaryVolume
   { primaryVolumeUnused1                     :: Word8                 -- ^ Always 0x00
   , primaryVolumeSystemIdentifier            :: StringA 32            -- ^ Name of the system that can act upon sectors 0x000-0x0F for the volume
   , primaryVolumeIdentifier                  :: StringD 32            -- ^ Identification of this volume
   , primaryVolumeUnused2                     :: Word64                -- ^ Always 0x00
   , primaryVolumeSpaceSize                   :: BothEndian Word32     -- ^ Number of logicel blocks in which the volume is recorded
   , primaryVolumeUnused3                     :: Vector 32 Word8       -- ^ All zeroes
   , primaryVolumeSetSize                     :: BothEndian Word16     -- ^ The size of the set in this logical volume (number of disks)
   , primaryVolumeSequenceNumber              :: BothEndian Word16     -- ^ The number of this disk in the Volume Set
   , primaryVolumeLogicalBlockSize            :: BothEndian Word16     -- ^ The size in bytes of a logical block
   , primaryVolumePathTableSize               :: BothEndian Word32     -- ^ The size in bytes of the path table
   , primaryVolumePathTableLocationLE         :: AsLittleEndian Word32 -- ^ LBA location of the path table containing only little-endian values
   , primaryVolumeOptPathTableLocationLE      :: AsLittleEndian Word32 -- ^ LBA location of the optional path table containing only little-endian values (0 if none)
   , primaryVolumePathTableLocationBE         :: AsBigEndian Word32    -- ^ LBA location of the path table containing only big-endian values
   , primaryVolumeOptPathTableLocationBE      :: AsBigEndian Word32    -- ^ LBA location of the optional path table containing only big-endian values (0 if none)
   , primaryVolumeRootDirectoryEntry          :: Vector 34 Word8       -- ^ actual Directory Record, which contains a single byte Directory Identifier (0x00)
   , primaryVolumeSetIdentifier               :: StringD 128           -- ^ Identifier of the volume set which this volume is a member
   , primaryVolumePublisherIdentifier         :: StringA 128           -- ^ The volume publisher. For extended publisher information, the first byte should be 0x5F, followed by the filename of a file in the root directory. If not specified, all bytes should be 0x20. The volume publisher. For extended publisher information, the first byte should be 0x5F, followed by the filename of a file in the root directory. If not specified, all bytes should be 0x20.
   , primaryVolumeDataPreparerIdentifier      :: StringA 128           -- ^ The identifier of the person(s) who prepared the data for this volume. For extended preparation information, the first byte should be 0x5F, followed by the filename of a file in the root directory. If not specified, all bytes should be 0x20.
   , primaryVolumeApplicationIdentifier       :: StringA 128           -- ^ Identifies how the data are recorded on this volume. For extended information, the first byte should be 0x5F, followed by the filename of a file in the root directory. If not specified, all bytes should be 0x20.
   , primaryVolumeCopyrightFileIdentifier     :: StringD 38            -- ^ Filename of a file in the root directory that contains copyright information for this volume set. If not specified, all bytes should be 0x20. Filename of a file in the root directory that contains copyright information for this volume set. If not specified, all bytes should be 0x20.
   , primaryVolumeAbstractFileIdentifier      :: StringD 36            -- ^ Filename of a file in the root directory that contains abstract information for this volume set. If not specified, all bytes should be 0x20.
   , primaryVolumeBibliographicFileIdentifier :: StringD 37            -- ^ Filename of a file in the root directory that contains bibliographic information for this volume set. If not specified, all bytes should be 0x20.
   , primaryVolumeCreationDate                :: DateTime              -- ^ the date and time of when the volume was created
   , primaryVolumeModificationDate            :: DateTime              -- ^ the date and time of when the volume was modified
   , primaryVolumeExpirationDate              :: DateTime              -- ^ The date and time after which this volume is considered to be obsolete. If not specified, then the volume is never considered to be obsolete
   , primaryVolumeEffectiveDate               :: DateTime              -- ^ The date and time after which the volume may be used. If not specified, the volume may be used immediately.
   , primaryVolumeFileStructureVersion        :: Word8                 -- ^ The directory records and path table version (always 0x01).
   , primaryVolumeUnused4                     :: Word8                 -- ^ Always 0x00
   , primaryVolumeCustomData                  :: Vector 512 Word8      -- ^ Contents not defined by ISO 9660
   , primaryVolumeReserved                    :: Vector 653 Word8      -- ^ Reserved by ISO
   }



// Remove the Authenticode signature from a Windows PE file (.exe, .dll).
//
// freememo-app.exe is a copy of Temurin's java.exe, which the Eclipse
// Foundation signed. rcedit changes the copy, so that signature no longer
// matches the file. A broken signature that names another signer is a false
// claim about our file, so the build removes it. (The "Publisher: Eclipse
// Adoptium" of the firewall prompt was the CompanyName of the version
// resource, which rcedit now sets.)
//
// The signature is the certificate table: data directory 4 of the optional
// header. Its "address" is a file offset, and the table is the end of the file.

import fs from 'node:fs'

// Pre:  file is a PE32 or PE32+ image.
// Post: data directory 4 and the header checksum are 0, and the file ends
//       where the certificate table began. Returns the bytes removed (0 when
//       the file had no signature). Throws when the file is not a PE image, or
//       when the table is not at the end of the file.
export function stripSignature (file) {
  const buf = fs.readFileSync(file)
  if (buf.readUInt16LE(0) !== 0x5a4d) throw new Error(`${file}: no MZ header`)
  const pe = buf.readUInt32LE(0x3c)
  if (buf.readUInt32LE(pe) !== 0x00004550) throw new Error(`${file}: no PE signature`)
  const opt = pe + 24
  const magic = buf.readUInt16LE(opt)
  const dirs = { 0x10b: opt + 96, 0x20b: opt + 112 }[magic]
  if (!dirs) throw new Error(`${file}: optional header magic 0x${magic.toString(16)}`)
  const entry = dirs + 4 * 8
  const offset = buf.readUInt32LE(entry)
  const size = buf.readUInt32LE(entry + 4)
  if (size === 0) return 0
  if (offset + size !== buf.length) {
    throw new Error(`${file}: the certificate table (${offset}+${size}) is not at the end (${buf.length})`)
  }
  buf.writeUInt32LE(0, entry)
  buf.writeUInt32LE(0, entry + 4)
  buf.writeUInt32LE(0, opt + 64) // CheckSum: an .exe needs none
  fs.writeFileSync(file, buf.subarray(0, offset))
  return size
}

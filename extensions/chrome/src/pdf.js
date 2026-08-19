// A minimal PDF writer: N JPEG tiles, one page each.
//
// Hand-written rather than a dependency. The whole job is a fixed object graph
// — catalog, pages, one page and one image XObject per tile — and a JPEG needs
// no re-encoding because PDF's DCTDecode filter takes JFIF bytes verbatim. A
// PDF library would carry a bundler with it for markup this shape.
//
// Tiles are JPEG for that reason. A PNG would need its samples inflated and
// re-deflated to become a PDF image, which is real work for a screenshot.

const A4_WIDTH_PT = 595.28;

/** ASCII → bytes. PDF syntax outside stream data is ASCII by construction. */
function ascii(str) {
  const out = new Uint8Array(str.length);
  for (let i = 0; i < str.length; i += 1) out[i] = str.charCodeAt(i) & 0xff;
  return out;
}

/**
 * Build a PDF from JPEG tiles, one tile per page.
 *
 * Pre : `tiles` is a non-empty array of {bytes: Uint8Array, width, height} in
 *       page order, each holding JFIF data. Every tile shares one pixel width.
 * Post: a Uint8Array holding a valid single-file PDF. Page width is A4's
 *       595.28pt; page height is the tile's own height at the same scale, so
 *       a short final tile yields a short final page rather than a blank band.
 * Inv : no tile is scaled, cropped or padded. Pixels map 1:1 onto the page.
 */
export function buildPdf(tiles) {
  if (!tiles || tiles.length === 0) throw new Error("buildPdf: no tiles");

  const scale = A4_WIDTH_PT / tiles[0].width;
  const objects = [];               // 1-based; objects[i] is object number i+1
  /** Append an object and return its PDF object number. */
  const push = (body) => {
    objects.push(body);
    return objects.length;
  };

  const catalogNum = push(null);    // patched below, once the pages id is known
  const pagesNum = push(null);

  const pageNums = [];
  for (const tile of tiles) {
    const imageNum = push({
      dict:
        `<< /Type /XObject /Subtype /Image /Width ${tile.width} ` +
        `/Height ${tile.height} /ColorSpace /DeviceRGB /BitsPerComponent 8 ` +
        `/Filter /DCTDecode /Length ${tile.bytes.length} >>`,
      stream: tile.bytes,
    });

    const widthPt = (tile.width * scale).toFixed(2);
    const heightPt = (tile.height * scale).toFixed(2);
    const content = ascii(`q ${widthPt} 0 0 ${heightPt} 0 0 cm /Im0 Do Q\n`);
    const contentNum = push({
      dict: `<< /Length ${content.length} >>`,
      stream: content,
    });

    pageNums.push(
      push({
        dict:
          `<< /Type /Page /Parent ${pagesNum} 0 R ` +
          `/MediaBox [0 0 ${widthPt} ${heightPt}] ` +
          `/Resources << /XObject << /Im0 ${imageNum} 0 R >> >> ` +
          `/Contents ${contentNum} 0 R >>`,
        stream: null,
      }),
    );
  }

  objects[catalogNum - 1] = {
    dict: `<< /Type /Catalog /Pages ${pagesNum} 0 R >>`,
    stream: null,
  };
  objects[pagesNum - 1] = {
    dict:
      `<< /Type /Pages /Count ${pageNums.length} ` +
      `/Kids [${pageNums.map((n) => `${n} 0 R`).join(" ")}] >>`,
    stream: null,
  };

  return serialize(objects);
}

/** Lay the object graph out as bytes and append a cross-reference table. */
function serialize(objects) {
  const chunks = [];
  let offset = 0;
  const write = (bytes) => {
    chunks.push(bytes);
    offset += bytes.length;
  };

  write(ascii("%PDF-1.4\n%âãÏÓ\n"));

  const offsets = [];
  objects.forEach((obj, index) => {
    offsets.push(offset);
    write(ascii(`${index + 1} 0 obj\n${obj.dict}\n`));
    if (obj.stream) {
      write(ascii("stream\n"));
      write(obj.stream);
      write(ascii("\nendstream\n"));
    }
    write(ascii("endobj\n"));
  });

  const xrefOffset = offset;
  let xref = `xref\n0 ${objects.length + 1}\n0000000000 65535 f \n`;
  for (const o of offsets) xref += `${String(o).padStart(10, "0")} 00000 n \n`;
  xref +=
    `trailer\n<< /Size ${objects.length + 1} /Root 1 0 R >>\n` +
    `startxref\n${xrefOffset}\n%%EOF\n`;
  write(ascii(xref));

  const out = new Uint8Array(offset);
  let at = 0;
  for (const chunk of chunks) {
    out.set(chunk, at);
    at += chunk.length;
  }
  return out;
}

/**
 * Cut a tall bitmap into A4-ratio tiles, top to bottom.
 *
 * Pre : `bitmap` is an ImageBitmap. Post: an array of {bytes, width, height}
 *       JPEG tiles. Every tile but the last is exactly A4-ratio tall. The last
 *       holds whatever remains and is NOT padded.
 * Inv : the tiles concatenate back to the source with no pixel repeated and
 *       none lost, because the cuts do not overlap.
 */
export async function tileToJpeg(bitmap, quality = 0.85) {
  const width = bitmap.width;
  const tileHeight = Math.max(1, Math.round(width * Math.SQRT2));
  const tiles = [];

  for (let top = 0; top < bitmap.height; top += tileHeight) {
    const height = Math.min(tileHeight, bitmap.height - top);
    const canvas = new OffscreenCanvas(width, height);
    const ctx = canvas.getContext("2d");
    // White ground: a JPEG has no alpha, and an unpainted canvas would
    // composite transparent pixels to black.
    ctx.fillStyle = "#ffffff";
    ctx.fillRect(0, 0, width, height);
    ctx.drawImage(bitmap, 0, top, width, height, 0, 0, width, height);
    const blob = await canvas.convertToBlob({ type: "image/jpeg", quality });
    tiles.push({
      bytes: new Uint8Array(await blob.arrayBuffer()),
      width,
      height,
    });
  }
  return tiles;
}

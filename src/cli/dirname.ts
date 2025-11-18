import { fileURLToPath } from 'url'
import { dirname as pathDirname } from 'path'

export const dirname = pathDirname(fileURLToPath(import.meta.url))

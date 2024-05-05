package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd3domain.wad.model.Wad;

public interface FileToZipFromWadService {
    FileZipper.FileToZip fromWad(Wad wad);
}

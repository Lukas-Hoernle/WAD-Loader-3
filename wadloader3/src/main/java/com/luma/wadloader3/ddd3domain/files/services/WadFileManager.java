package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd4abstraction.functional.Either;
import com.luma.wadloader3.ddd4abstraction.functional.ErrorMessage;
import org.springframework.web.multipart.MultipartFile;

/**
 * FileManager for WadFiles.
 * */
public interface WadFileManager {
    Either<ErrorMessage, FilePath> saveFile(String wadName, MultipartFile wadFile);

    Either<ErrorMessage, FilePath> findFileByName(String name);
}
